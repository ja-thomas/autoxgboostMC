#' @title Result of an autoxgboost call.
#'
#' @description
#' Result of \code{\link{autoxgboost}}. A R6 class containing the following elements:
#' \describe{
#'   \item{optim_result [\code{\link[mlrMBO]{MBOSingleObjResult}}]}{Optimization result object. See: \code{\link[mlrMBO]{MBOSingleObjResult}}}
#'   \item{final_learner [\code{\link[mlr]{Learner}}]}{Xgboost learner with best found hyper paramater configuration.}
#'   \item{final_model [\code{\link[mlr]{WrappedModel}} | \code{NULL}]}{If \code{build.final.model=TRUE} in \code{\link{autoxgboost}} a \pkg{mlr} model build by the full dataset and \code{final.learner}.}
#'   \item{measures [\code{\link[mlr]{Measure}}]}{Measure used for optimization.}
#' }
#' And the following functions:\cr
#' \describe{
#'   \item{.$predict(newdata)}
#'   \item{.$get_tune_results()}
#'   \item{.$plot_pareto_front()}
#' }
#' @name AutoxgbResult
#' @rdname AutoxgbResult
AutoxgbResult = R6::R6Class("AutoxgbResult",
  public = list(
  optim_result = NULL,
  final_learner = NULL,
  final_model = NULL,
  measures = NULL,
  preproc_pipeline = NULL,
  initialize = function(optim_result, final_learner, final_model, measures, preproc_pipeline) {
    self$optim_result = optim_result
    self$final_learner = final_learner
    self$final_model = final_model
    self$measures = measures
    self$preproc_pipeline = preproc_pipeline
  },
  print = function(...) {
    op = self$optim_result$opt.path
    pars = trafoValue(op$par.set, self$optim_result$x)
    pars$nrounds = self$get_best_from_opt("nrounds")
    thr = self$get_best_from_opt(".threshold")
    catf("Autoxgboost tuning result")
    catf("Recommended parameters:")
    for (p in names(pars)) {
      if (p == "nrounds" || isInteger(op$par.set$pars[[p]])) {
        catf("%s: %i", stringi::stri_pad_left(p, width = 17), as.integer(pars[p]))
      } else if (isNumeric(op$par.set$pars[[p]], include.int = FALSE)) {
        catf("%s: %.3f", stringi::stri_pad_left(p, width = 17), pars[p])
      } else {
        catf("%s: %s", stringi::stri_pad_left(p, width = 17), pars[p])
      }
    }
    catf("\n\nPreprocessing pipeline:")
        print(self$preproc_pipeline)
    # FIXME: Nice Printer for results:
    catf("\nWith tuning result:")
    for (i in seq_along(self$measures)) catf("    %s = %.3f", self$measures[[i]]$id, self$optim_result$y[[i]])

    if (!is.null(thr)) {
      if (length(thr) == 1) {
        catf("\nClassification Threshold: %.3f", thr)
      } else {
        catf("\nClassification Thresholds: %s", paste(names(thr), round(thr, 3), sep = ": ", collapse = "; "))
      }
    }
  },
  predict = function(newdata) {
    if (is.null(self$final_model))
      stop("Final model was not build, use best param configs to build the model yourself.")
    predict(self$final_model, newdata = newdata, ...)
  },
  get_tune_results = function() {
    as.data.frame(self$optim_result$opt.path)
  },
  plot_pareto_front = function(x = NULL, y = NULL, color = NULL) {
    df = self$get_tune_results()
    assert_choice(x, colnames(df), null.ok = TRUE)
    assert_choice(y, colnames(df), null.ok = TRUE)
    assert_choice(color, colnames(df), null.ok = TRUE)
    if (is.null(x)) x = self$measure_ids[1]
    if (is.null(y) & length(self$measures) >= 2L) y = self$measure_ids[2]
    
    p = ggplot2::ggplot(df, aes_string(x = x, y = y, color = color)) +
    geom_point() +
    theme_bw()

    return(p)
  },
  get_best_from_opt = function(what) {
    self$optim_result$opt.path$env$extra[[self$optim_result$best.ind]][[what]]
  }
  ),
  active = list(
    measure_ids = function() {
      sapply(self$measures, function(x) x$id)
    }
  )
)