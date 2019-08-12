context("Summary Info Testing")

# load the libraries
library(ModelComparison)

test_that("Add predictions to df for training with stacking", {
  # helper function for a 2 response iris dataset
  iris <- PrepareIris()
  # create the models
  comp <- GetModelComparisons(iris[, 1:4], iris[, 5])

  # test ModelComparison
  summary <- "This object is a comparison of the following models:\nsvmLinear, glmnet"
  expect_equal(capture_output(summary(comp, extra=F)), summary)
  # test ModelComparison without accuracy:
  # TODO?

  # test Ensemble
  ensem <- Ensemble(comp$model.list, "majorityWeight", iris[, 1:4], iris[, 5])
  summary <- "This object is a Ensemble of the following models: \nsvmLinear, glmnet\n\nAnd uses a majorityWeight voting type"
  expect_equal(capture_output(summary(ensem)), summary)

})

