context("Voting and related functionality")

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

test_that("Dataset predictions on Majority Vote", {
  # prepare the dataset
  iris_ready <- prepare_iris()
  # create the models
  comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5])
  preds <- predict(comp, iris_ready[,1:4])
  pred_list <- MajorityVote(preds)
})

test_that("Dataset predictions on Majority Weight", {
  # prepare the dataset
  iris_ready <- prepare_iris()
  # create the models
  comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5])
  pred <- predict(comp, iris_ready[,1:4])
  # turn pred list of DF's into a real list of values
  pred <- StripPredictions(pred)
  pred_list <- MajorityWeight(pred, runif(n=length(comp$model_list), min=0, max=1))
})

test_that("Dataset predictions on Average Vote", {
  # prepare the dataset
  iris_ready <- prepare_iris()
  # create the models
  comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5])
  pred <- predict(comp, iris_ready[,1:4])
  # turn pred list of DF's into a real list of values
  pred <- StripPredictions(pred)
  pred_list <- AverageVote(pred)
})


test_that("Verify Majority Voting function", {
  pred1 <- list(0,0,0,0,0,0,0,0)
  pred2 <- list(0,0,0,0,0,0,0,0)
  pred3 <- list(1,1,1,1,1,1,1,1)
  pred4 <- list(1,1,1,1,1,1,1,1)
  pred5 <- list(0,1,0,1,0,1,0,1)
  list_of_preds <- list(pred1, pred2, pred3, pred4, pred5)
  voted <- MajorityVote(list_of_preds)
  expect_equal(voted, c(0,1,0,1,0,1,0,1))
  })

test_that("Majority Voting on NA's sends an error", {
  pred1 <- list(0,0,0,0,0,0,0,0)
  pred2 <- list(0,0,0,0,0,0,0,0)
  pred3 <- list(1,1,1,1,1,1,1,1)
  pred4 <- list(1,1,1,1,1,1,1,1)
  pred5 <- list(0,1,0,1,0,1,0,NA)
  list_of_preds <- list(pred1, pred2, pred3, pred4, pred5)
  expect_error(MajorityVote(list_of_preds),
               "There are NA's in this prediction.  Please predict correct classes",
               fixed=TRUE)
})

test_that("Verify Weighting Voting function", {
  pred1 <- list(.5,.5,.5,.5,.5)
  pred2 <- list(.99,.99,.99,.99,.99)
  pred3 <- list(.1, 0, .1, .8, 0.001)
  list_of_preds <- list(pred1, pred2, pred3)
  weights = list(0.5, .25, .8)
  voted <- MajorityWeight(list_of_preds, weights)
  expect_equal(voted, c(0.5775, 0.4975, 0.5775, 1, .4983))
})

test_that("Majority Weighting on NA's sends an error", {
  pred1 <- list(.5,.5,.5,.5,.5)
  pred2 <- list(.99,.99,.99,.99,NA)
  pred3 <- list(.1, 0, .1, .8, 0.001)
  list_of_preds <- list(pred1, pred2, pred3)
  weights = list(0.5, .25, .8)
  expect_error(MajorityWeight(list_of_preds, weights), "There are NA's in this prediction.  Please predict correct classes",
               fixed=TRUE)
})

test_that("Verify Average Voting function", {
  pred1 <- list(.5,.5,.5,.5,.5)
  pred2 <- list(.99,.99,.99,.99,.99)
  pred3 <- list(.1, 0, .1, .8, 0.001)
  list_of_preds <- list(pred1, pred2, pred3)
  voted <- AverageVote(list_of_preds)
  expect_equal(voted, c(0.53, 1.49 / 3, 0.53, 2.29 / 3, 0.497))
})

test_that("Average Weighting on NA's sends an error", {
  pred1 <- list(.5,.5,.5,.5,.5)
  pred2 <- list(.99,.99,.99,.99,NA)
  pred3 <- list(.1, 0, .1, .8, 0.001)
  list_of_preds <- list(pred1, pred2, pred3)
  expect_error(AverageVote(list_of_preds), "There are NA's in this prediction.  Please predict correct classes",
               fixed=TRUE)
})

