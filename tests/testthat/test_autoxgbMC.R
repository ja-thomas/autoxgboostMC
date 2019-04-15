context("AutoxgboostMC")
test_that("autoxgboostMC works on different tasksfor single measure",  {

  tasks = list(
    sonar.task, # binary classification
    iris.fac,   # binary classification with factors
    iris.task,  # multiclass classification
    subsetTask(bh.task, subset = 1:50),
    iris.fac)

  for (t in tasks) {
    axgb = AutoxgboostMC$new(measures = list(acc))
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
    iris.task)  # multiclass classification

  for (t in tasks) {
    axgb = AutoxgboostMC$new(measures = list(acc, timepredict))
    axgb$fit(t, time.budget = 10L)
    expect_true(!is.null(axgb$model))
    p = axgb$predict(t)
    expect_class(p, "Prediction")
  }
})

test_that("Multiple measures work",  {
    fairf11 = setMeasurePars(fairf1, grouping = function(df) as.factor(df$age > 30))
    axgb = AutoxgboostMC$new(measures = list(acc, fairf11))
    axgb$fit(pid.task, time.budget = 10L)
    expect_true(!is.null(axgb$model))
    p = axgb$predict(pid.task)
    expect_class(p, "Prediction")
})

test_that("New measures work",  {
    fairf11 = setMeasurePars(fairf1, grouping = function(df) as.factor(df$age > 30))
    axgb = AutoxgboostMC$new(measures = list(acc, fairf11, sparsity))
    axgb$fit(pid.task, time.budget = 10L)
    expect_true(!is.null(axgb$model))
    p = axgb$predict(pid.task)
    expect_class(p, "Prediction")
})



