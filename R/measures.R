#' Squared difference in F1 Scores between groups
#' @export
fairf1 = mlr::makeMeasure(id = "fairness.f1", minimize = TRUE, properties = c("classif", "response"),
  extra.args = list(groupvar = NULL), best = 0, worst = 1,
  fun = function(task, model, pred, feats, extra.args) {
    fs = aggregate(as.formula(paste0("truth + response ~ ", extra.args$var)), pred$data, 
      function(truth, response) {
        measureF1(truth, response, pred$task.desc$positive)
    })
    (sum(fs - mean(fs)))^2
  }
)

#' Squared difference in False Postive rate between groups
#' @export
fairfpr = mlr::makeMeasure(id = "fairness.f1", minimize = TRUE, properties = c("classif", "response"),
  extra.args = list(groupvar = NULL), best = 0, worst = 1,
  fun = function(task, model, pred, feats, extra.args) {
    fs = aggregate(as.formula(paste0("truth + response ~ ", extra.args$groupvar)), pred$data, 
      function(truth, response) {
        measureFPR(truth, response, pred$task.desc$positive)
    })
    (sum(fs - mean(fs)))^2
  }
)


#' Sparsity as percentage of used features
#' @export
xgb.sparsity = mlr::makeMeasure(id = "sparse.xgb", minimize = TRUE, properties = c("classif", "response"),
  extra.args = list(), best = 0, worst = 1L,
  fun = function(task, model, pred, feats, extra.args) {
    if(model$learner$package != "xgboost") stop("Measure sparse.xgb is only available for xgboost models")
    nfeats = setdiff(unique(xgboost::xgb.model.dt.tree(model = model$learner.model)$Feature), "Leaf")
    nfeats / sum(model$task.desc$n.feat)
  }
)

# FIXME: Implement Christoph's IML Measures.
# FIXME: Implement some more fairness measures
# FIMXE: Any other measure?
