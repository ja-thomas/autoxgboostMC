#' @title Fit and optimize a xgboost model for multiple criteria
#'
#' @description
#' An xgboost model is optimized based on a set of measures (see [\code{\link[mlr]{Measure}}]).
#' The bounds of the parameter in which the model is optimized, are defined by \code{\link{autoxgbparset}}.
#' For the optimization itself bayesian optimization with \pkg{mlrMBO} is used.
#' Without any specification of the control object, the optimizer runs for for 80 iterations or 1 hour,
#' whichever happens first.
#' Both the parameter set and the control object can be set by the user.
#'
#' Arguments to `.$new()`
#' @param measures [list of \code{\link[mlr]{Measure}}]\cr
#'   Performance measure. If \code{NULL} \code{\link[mlr]{getDefaultMeasure}} is used.
#' @param parset [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set to tune over. Default is \code{\link{autoxgbparset}}.
#'   Can be updated using `.$set_parset()`.
#' @param nthread [integer(1)]\cr
#'   Number of cores to use.
#'   If \code{NULL} (default), xgboost will determine internally how many cores to use.
#'   Can be set using `.$set_nthread()`.
#'
#' Arguments to `.$fit()`:
#' @param task [\code{\link[mlr]{Task}}]\cr
#'   The task to be trained.
#' @param iterations [\code{integer(1L}]\cr
#'   Number of MBO iterations to do. Will be ignored if custom \code{control} is used.
#'   Default is \code{160}.
#' @param time.budget [\code{integer(1L}]\cr
#'   Time that can be used for tuning (in seconds). Will be ignored if custom \code{control} is used.
#'   Default is \code{3600}, i.e., one hour.
#' @param build.final.model [\code{logical(1)}]\cr
#'   Should the model with the best found configuration be refitted on the complete dataset?
#'   Default is \code{FALSE}.
#' @param control [\code{\link[mlrMBO]{MBOControl}}]\cr
#'   Control object for optimizer.
#'   If not specified, the default \code{\link[mlrMBO]{makeMBOControl}}] object will be used with
#'   \code{iterations} maximum iterations and a maximum runtime of \code{time.budget} seconds.
#'
#' Additional Arguments:
#' @param mbo.learner [\code{\link[mlr]{Learner}}]\cr
#'   Regression learner from mlr, which is used as a surrogate to model our fitness function.
#'   If \code{NULL} (default), the default learner is determined as described here:
#'   \link[mlrMBO]{mbo_default_learner}.
#'   Can be set using `.$set_mbo_learner()`.
#' @param design.size [\code{integer(1)}]\cr
#'   Size of the initial design. Default is \code{15L}.
#'   Can be set via `.$set_design_size()`
#' @param max.nrounds [\code{integer(1)}]\cr
#'   Maximum number of allowed boosting iterations. Default is \code{10^6}.
#'   Can be set via `.$set_max_nrounds()`.
#' @param early.stopping.rounds [\code{integer(1L}]\cr
#'   After how many iterations without an improvement in the boosting OOB error should be stopped?
#'   Default is \code{10}.
#'   Can be set via `.$set_early_stopping_rounds()`.
#' @param early.stopping.fraction [\code{numeric(1)}]\cr
#'   What fraction of the data should be used for early stopping (i.e. as a validation set).
#'   Default is \code{4/5}.
#'   Can be set via `.$set_early_stopping_fraction()`.
#' @param impact.encoding.boundary [\code{integer(1)}]\cr
#'   Defines the threshold on how factor variables are handled. Factors with more levels than the \code{"impact.encoding.boundary"} get impact encoded while factor variables with less or equal levels than the \code{"impact.encoding.boundary"} get dummy encoded.
#'   For \code{impact.encoding.boundary = 0L}, all factor variables get impact encoded while for \code{impact.encoding.boundary = .Machine$integer.max}, all of them get dummy encoded.
#'   Default is \code{10}.
#'   Can be set via `.$set_impact_encoding_boundary()`.
#' @param tune.threshold [logical(1)]\cr
#'   Should thresholds be tuned? This has only an effect for classification, see \code{\link[mlr]{tuneThreshold}}.
#'   Default is \code{TRUE}.
#'   Can be set via `.$set_tune_threshold()`.
#'
#' @export
#' @examples
#' \donttest{
#' iris.task = makeClassifTask(data = iris, target = "Species")
#' axgb = AutoxgboostMC$new(measure = auc)
#' axgb$fit(t, time.budget = 5L)
#' p = axgb$predict(iris.task)
#' }
AutoxgboostMC = R6::R6Class("AutoxgboostMC",
  public = list(
    measures = NULL,

    control = NULL,
    parset = NULL,
    design.size = 15L,
    mbo.learner = NULL,
    iterations = NULL,
    time.budget = NULL,

    max.nrounds = 3*10^3L,
    early.stopping.rounds = 10L,
    early.stopping.fraction = 4/5,
    impact.encoding.boundary = 10L,
    tune.threshold = TRUE, # FIXME: Set this to FALSE if measure does not work with prob?
    nthread = NULL,

    model = NULL,
    build.final.model = NULL,
    total_iterations = 0L,
    total_time = 0L,

    initialize = function(measures = NULL, parset = NULL, nthread = NULL) {
      assert_list(measures, types = "Measure", null.ok = TRUE)
      assert_class(parset, "ParamSet", null.ok = TRUE)
      # Set defaults
      self$measures = coalesce(measures, list(getDefaultMeasure(task)))
      self$parset = coalesce(parset, autoxgboostMC::autoxgbparset)
      self$nthread = assert_integerish(nthread, lower = 1, len = 1L, null.ok = TRUE)
    },

    print = function(...) {
      catf("AutoxgboostMC Learner")
      catf("Trained: %s", ifelse(is.null(self$model), "no", "yes"))
    },

    fit = function(task, iterations = 160L, time.budget = 3600L, build.final.model = TRUE, control = NULL) {
      assert_class(task, "SupervisedTask")
      assert_class(control, "MBOControl", null.ok = TRUE)
      self$iterations = assert_integerish(iterations)
      self$time.budget = assert_integerish(time.budget)
      self$build.final.model = assert_flag(build.final.model)
      # Set defaults
      if (is.null(control)) {
        measures_ids = sapply(self$measures, function(x) x$id)
        ctrl = makeMBOControl(n.objectives = length(measures_ids), y.name = measures_ids)
        self$control = setMBOControlTermination(ctrl, iters = iterations,
          time.budget = time.budget)
      }
      if (length(self$measures) == 1L) {
        # For now we delegate to autoxgboost
        self$model = autoxgboost(task, measure = self$measures[[1]], control = self$control,
          iterations = iterations, time.budget = time.budget, par.set = self$parset,
          max.nrounds = self$max.nrounds, early.stopping.rounds = self$early.stopping.rounds,
          early.stopping.fraction = self$early.stopping.fraction,
          build.final.model = build.final.model, design.size = self$design.size,
          impact.encoding.boundary = self$impact.encoding.boundary,
          mbo.learner = self$mbo.learner, nthread = self$nthread,
          tune.threshold = self$tune.threshold)
      } else {
        self$model = autoxgboostmc(task, measures = self$measures, control = self$control,
          iterations = iterations, time.budget = time.budget, par.set = self$parset,
          max.nrounds = self$max.nrounds, early.stopping.rounds = self$early.stopping.rounds,
          early.stopping.fraction = self$early.stopping.fraction,
          build.final.model = build.final.model, design.size = self$design.size,
          impact.encoding.boundary = self$impact.encoding.boundary,
          mbo.learner = self$mbo.learner, nthread = self$nthread,
          tune.threshold = self$tune.threshold)
      }
      self$total_iterations = self$total_iterations + self$iterations
      self$total_time = self$total_time + self$time.budget
    },

    predict = function(newdata) {
      predict(self$model$final.model, newdata)
    },

    set_max_nrounds = function(value) {
       self$max.nrounds = assert_integerish(value, lower = 1L, len = 1L)
    },
    set_early_stopping_rounds = function(value) {
       self$early.stopping.rounds = assert_integerish(value, lower = 1L, len = 1L)
    },
    set_early_stopping_fraction = function(value) {
      self$early.stopping.fraction = assert_numeric(early.stopping.fraction, lower = 0, upper = 1, len = 1L)
    },
    set_design_size = function(value) {
      self$design.size = assert_integerish(design.size, lower = 1L, len = 1L)
    },
    set_tune_threshold = function(value) {
      self$tune.threshold = assert_flag(value)
    },
    set_impact_encoding_boundary = function(value) {
      self$impact_encoding_boundary = assert_integerish(value, lower = 0, len = 1L)
    },
    set_nthread = function(value) {
      self$nthread = assert_integerish(value, lower = 1, len = 1L, null.ok = TRUE)
    },
    set_measures = function(value) {
      self$measures = assert_list(value, types = "Measure", null.ok = TRUE)
    },
    set_parset = function(value) {
      self$parset = assert_class(value, "ParamSet", null.ok = TRUE)
    }
  )
)





