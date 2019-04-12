context("AutoxgboostMC")
test_that("autoxgboostMC works on different tasksfor single measure",  {

  tasks = list(
    sonar.task, # binary classification
    iris.fac,   # binary classification with factors
    iris.task,  # multiclass classification
    subsetTask(bh.task, subset = 1:50),
    iris.fac)

  for (t in tasks) {
    axgb = AutoxgboostMC$new(measure = list(acc))
    axgb$fit(t, time.budget = 5L)
    expect_true(!is.null(axgb$model))
    p = axgb$predict(t)
    expect_class(p, "Prediction")
  }

})

test_that("autoxgboostMC works on different tasks",  {

  tasks = list(
    sonar.task, # binary classification
    iris.fac,   # binary classification with factors
    iris.task,  # multiclass classification
    subsetTask(bh.task, subset = 1:50),
    iris.fac)

  for (t in tasks) {
    axgb = AutoxgboostMC$new(measure = list(acc, timetrain))
    axgb$fit(t, time.budget = 5L)
    expect_true(!is.null(axgb$model))
    p = axgb$predict(t)
    expect_class(p, "Prediction")
  }
})


test_that("New measures work",  {
    axgb = AutoxgboostMC$new(measures = list(acc, timetrain, fairf1, xgb.sparsity))
    axgb$fit(pid.task, time.budget = 5L)
    expect_true(!is.null(axgb$model))
    p = axgb$predict(pid.task)
    expect_class(p, "Prediction")
})
