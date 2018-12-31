context("Voting with Ensemble Functionality")

# load the libraries
library(BestModel)

# helper function for a 2 response iris dataset
prepare_iris <- function() {
  data(iris)
  toBeRemoved<-which(iris$Species=="setosa")
  irisReal <-iris[-toBeRemoved,]
  irisReal <- droplevels(irisReal)
  levels(irisReal$Species) <- c('versicolor', 'virginica' )
  return(irisReal)
}

test_that("Ensemble weighting function with auto-weighting", {
  # prepare the dataset
  iris_ready <- prepare_iris()
  # create the models
  comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5])
  ensem <- Ensemble(comp$model_list, "majorityWeight", iris_ready[,1:4], iris_ready[,5])
  ensem$weight.list
  expect_equal(class(ensem), "Ensemble")
  pred <- predict(ensem, iris_ready[, 1:4])
  expect_equal(length(iris_ready[, 5]), length(pred))
})

test_that("Average weighting Ensemble", {
  # prepare the dataset
  iris_ready <- prepare_iris()
  # create the models
  comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5])
  ensem <- Ensemble(comp$model_list, "averageVote")
  ensem$weight.list
  expect_equal(class(ensem), "Ensemble")
  pred <- predict(ensem, iris_ready[, 1:4])
  expect_equal(length(iris_ready[, 5]), length(pred))
})

test_that("Majority Vote Ensemble", {
  # prepare the dataset
  iris_ready <- prepare_iris()
  # create the models
  comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5])
  ensem <- Ensemble(comp$model_list, "majorityVote")
  ensem$weight.list
  expect_equal(class(ensem), "Ensemble")
  pred <- predict(ensem, iris_ready[, 1:4])
  expect_equal(length(iris_ready[, 5]), length(pred))
})
