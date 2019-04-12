#' @title Fit and optimize a xgboost model for multiple criteria.
#'
#' @description
#' For params and a more in-depth description see `?autoxgboost`.
#'
#' @param measures [\code{list of \link[mlr]{Measure}}]\cr
#'   Performance measures. If \code{NULL} \code{\link[mlr]{getDefaultMeasure}} is used.
#'
#' @return \code{\link{AutoxgbResult}}
#' @export
#' @examples
#' \donttest{
#' iris.task = makeClassifTask(data = iris, target = "Species")
#' ctrl = makeMBOControl()
#' ctrl = setMBOControlTermination(ctrl, iters = 1L) #Speed up Tuning by only doing 1 iteration
#' res = autoxgboostmc(iris.task, control = ctrl, measures = list(mmce))
#' res
#' }
autoxgboostmc = function(task, measures = NULL, control = NULL, iterations = 160L, time.budget = 3600L,
  par.set = NULL, max.nrounds = 10^6, early.stopping.rounds = 10L, early.stopping.fraction = 4/5,
  build.final.model = TRUE, design.size = 15L, impact.encoding.boundary = 10L, mbo.learner = NULL,
  nthread = NULL, tune.threshold = TRUE) {

  # check inputs
  assertClass(task, "SupervisedTask")
  assertList(measures, type = "Measure", null.ok = TRUE)
  assertClass(control, "MBOControl", null.ok = TRUE)
  assertIntegerish(iterations)
  assertIntegerish(time.budget)
  assertClass(par.set, "ParamSet", null.ok = TRUE)
  assertIntegerish(max.nrounds, lower = 1L, len = 1L)
  assertIntegerish(early.stopping.rounds, lower = 1L, len = 1L)
  assertNumeric(early.stopping.fraction, lower = 0, upper = 1, len = 1L)
  assertFlag(build.final.model)
  assertIntegerish(design.size, lower = 1L, len = 1L)
  assertIntegerish(impact.encoding.boundary, lower = 0, len = 1L)
  assertIntegerish(nthread, lower = 1, len = 1L, null.ok = TRUE)
  assertFlag(tune.threshold)

  # Check whether the measure(s) make sense with thresholding

  is_thresholded_measure = sapply(measures, function(x) {
    props = getMeasureProperties(x)
    any(props == "req.truth") & !any(props == "req.prob")
  })
  if (!any(is_thresholded_measure) & tune.threshold) {
    warning("Threshold tuning is active, but no measure for tuning thresholds!
      Skipping threshold tuning!")
    tune.threshold = FALSE
  }

  # make_baselearner
  req_prob_measure = sapply(measures, function(x) {
    any(getMeasureProperties(x) == "req.prob")
  })
  tt = getTaskType(task)
  td = getTaskDesc(task)
  has.cat.feats = sum(td$n.feat[c("factors", "ordered")]) > 0
  pv = list()
  if (!is.null(nthread))
    pv$nthread = nthread

  # create base.learner
  if (tt == "classif") {
    predict.type = ifelse(any(req_prob_measure) | tune.threshold, "prob", "response")
    if(length(td$class.levels) == 2) {
      objective = "binary:logistic"
      eval_metric = "error"
      par.set = c(par.set, makeParamSet(makeNumericParam("scale_pos_weight", lower = -10, upper = 10, trafo = function(x) 2^x)))
    } else {
      objective = "multi:softprob"
      eval_metric = "merror"
    }

    base.learner = makeLearner("classif.xgboost.earlystop", id = "classif.xgboost.earlystop", predict.type = predict.type,
      eval_metric = eval_metric, objective = objective, early_stopping_rounds = early.stopping.rounds, maximize = !measures[[1]]$minimize,
      max.nrounds = max.nrounds, par.vals = pv)
  } else if (tt == "regr") {
    predict.type = NULL
    objective = "reg:linear"
    eval_metric = "rmse"

    base.learner = makeLearner("regr.xgboost.earlystop", id = "regr.xgboost.earlystop",
      eval_metric = eval_metric, objective = objective, early_stopping_rounds = early.stopping.rounds, maximize = !measures[[1]]$minimize,
      max.nrounds = max.nrounds, par.vals = pv)

  } else {
    stop("Task must be regression or classification")
  }

  ## make_pipeline -----
  # Create pipeline
  preproc.pipeline = NULLCPO

  #if (!is.null(task$feature.information$timestamps))
  #  preproc.pipeline %<>>% cpoExtractTimeStampInformation(affect.names = unlist(task$feature.information$timestamps))
  if (has.cat.feats) {
    preproc.pipeline %<>>% generateCatFeatPipeline(task, impact.encoding.boundary)
  }
  preproc.pipeline %<>>% cpoDropConstants()


  # process data and apply pipeline
  # split early stopping data
  rinst = makeResampleInstance(makeResampleDesc("Holdout", split = early.stopping.fraction), task)
  task.test = subsetTask(task, rinst$test.inds[[1]])
  task.train = subsetTask(task, rinst$train.inds[[1]])

  task.train %<>>% preproc.pipeline
  task.test %<>>% retrafo(task.train)
  base.learner = setHyperPars(base.learner, early.stopping.data = task.test)

  #-- optimize
  # Optimize
  opt = smoof::makeMultiObjectiveFunction(name = "optimizeWrapperMultiCrit",
    fn = function(x) {
      x = x[!vlapply(x, is.na)]
      lrn = setHyperPars(base.learner, par.vals = x)
      mod = train(lrn, task.train)
      pred = predict(mod, task.test)
      nrounds = getBestIteration(mod)

      # FIXME: We might want to tune the threshold for all (?) measures?
      # For now we tune threshold of first applicable one.
      if (tune.threshold && getTaskType(task.train) == "classif") {
        tune.res = tuneThreshold(pred = pred, measure = measures[is_thresholded_measure][[1]])
        # FIXME: Does order matter?
        res = c(performance(pred, measures[!is_thresholded_measure], model = mod, task = task), tune.res$perf)
        attr(res, "extras") = list(nrounds = nrounds, .threshold = tune.res$th)
      } else {
        res = performance(pred, measures, model = mod, task = task)
        attr(res, "extras") = list(nrounds = nrounds)
      }
      return(res)
    },
    par.set = par.set, noisy = FALSE, has.simple.signature = FALSE, minimize =  sapply(measures, function(x) x$minimize),
    n.objectives = length(measures)
  )
  # Multicrit stuff
  control = setMBOControlMultiObj(control, method = "dib",dib.indicator = "eps")
  control = setMBOControlInfill(control, crit = makeMBOInfillCritDIB(cb.lambda = 2L))
  des = generateDesign(n = design.size, par.set)
  optim.result = mbo(fun = opt, control = control, design = des, learner = mbo.learner)

  #---- finalize
  lrn = buildFinalLearner(optim.result, objective, predict.type, par.set = par.set, preproc.pipeline = preproc.pipeline)

  mod = NULL
  if(build.final.model) mod = train(lrn, task)

  makeS3Obj("AutoxgbResult",
    optim.result = optim.result,
    final.learner = lrn,
    final.model = mod,
    measure = measures,
    preproc.pipeline = preproc.pipeline
  )
}
