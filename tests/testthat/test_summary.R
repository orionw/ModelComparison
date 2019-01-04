context("Summary Info Testing")

# load the libraries
library(BestModel)

prepare_iris <- function() {
  data(iris)
  toBeRemoved<-which(iris$Species=="setosa")
  irisReal <-iris[-toBeRemoved,]
  irisReal <- droplevels(irisReal)
  levels(irisReal$Species) <- c('versicolor', 'virginica' )
  return(irisReal)
}

test_that("Add predictions to df for training with stacking", {
  # helper function for a 2 response iris dataset
  iris_ready <- prepare_iris()
  # create the models
  comp <- getModelComparisons(iris_ready[, 1:4], iris_ready[, 5])

  # test ModelComparison
  summary <- "This object is a comparison of the following models:\nsvmLinear, glmnet\n\nwith respective accuracies of:\n0.932101667845121 0.949745146233077 "
  expect_equal(capture_output(summary(comp)), summary)
  # test ModelComparison without accuracy:
  # TODO?

  # test Ensemble
  ensem <- Ensemble(comp$model_list, "majorityWeight", iris_ready[, 1:4], iris_ready[, 5])
  summary <- "This object is a Ensemble of the following models: \nsvmLinear, glmnet\n\nAnd uses a majorityWeight voting type"
  expect_equal(capture_output(summary(ensem)), summary)

})


output <- function() {
  cat("A\nA")
}
expect_that(output(), prints_text("A\nA"))

