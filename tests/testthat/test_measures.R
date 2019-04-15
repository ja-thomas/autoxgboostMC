context("Measures")
test_that("Multiple measures work",  {
  fairf11 = setMeasurePars(fairf1, grouping = function(df) as.factor(df$age > 30))
  lrn = makeLearner("classif.rpart")
  mod = train(lrn, pid.task)
  prd = predict(mod, pid.task)
  performance(prd, fairf11, task = pid.task)
})
