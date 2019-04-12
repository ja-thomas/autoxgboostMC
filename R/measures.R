#' Squared difference in F1 Scores between groups
#' @export
fairf1 = mlr::makeMeasure(id = "fairness.f1", minimize = TRUE, properties = c("classif", "response"),
  extra.args = list(),
  fun = function(task, model, pred, feat, extra.args) {
    # group is a logical vector indicating group1 or group2
    g1 = pred$data[extra.args$extra.args$group, ]
    g2 = pred$data[!extra.args$extra.args$group, ]
    g1f1 = measureF1(g1$truth, g1$response, pred$task.desc$positive)
    g2f1 = measureF1(g2$truth, g2$response, pred$task.desc$positive)
    (g1f1 - g2f1)^2
  }
)



#' Sparsity as percentage of available features
#' @export
xgb.sparsity = mlr::makeMeasure(id = "sparse.xgb", minimize = TRUE, properties = c("classif", "response"),
  extra.args = list(),
  fun = function(task, model, pred, extra.args) {
    if(model$learner$package != "xgboost") stop("Measure sparse.xgb is only available for xgboost models")
    nfeats = setdiff(unique(xgboost::xgb.model.dt.tree(model = model$learner.model)$Feature), "Leaf")
    nfeats / sum(model$task.desc$n.feat)
  }
)

# FIXME: Implement Christoph's IML Measures.
# FIXME: Implement some more fairness measures
# FIMXE: Any other measure?
