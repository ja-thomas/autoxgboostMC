context("AutoxgboostMC")
test_that("autoxgboostMC works on different tasks",  {

  tasks = list(
    sonar.task, # binary classification
    iris.fac,   # binary classification with factors
    iris.task,  # multiclass classification
    subsetTask(bh.task, subset = 1:50),
    iris.fac)

  for (t in tasks) {
    axgb = AutoxgboostMC$new(measure = auc)
    axgb$fit(t, time.budget = 5L)
    expect_true(!is.null(axgb$model))
    p = axgb$predict(iris.task)
    expect_class(p, "Prediction")
  }
})
