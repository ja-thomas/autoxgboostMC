Sys.setenv("R_TESTS" = "")

library(testthat)
library(checkmate)
library(autoxgboostMC)

test_check("autoxgboostMC")
