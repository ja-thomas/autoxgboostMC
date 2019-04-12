# Squared difference in F1 Scores between groups
fairf1 = mlr::makeMeasure(id = "fairness.f1", minimize = TRUE, properties = c("classif", "response"),
  fun = function(task, model, pred, extra.args) {
  # group is a logical vector indicating group1 or group2
  g1 = pred$data[extra.args$group]
  g2 = pred$data[!extra.args$group]
  g1f1 = measureF1(g1$data$truth, g1$data$response, pred$task.desc$positive)
  g2f1 = measureF1(g2$data$truth, g2$data$response, pred$task.desc$positive)
  (g1f1 - g2f1)^2
  }
)

