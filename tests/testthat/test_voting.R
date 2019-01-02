context("Voting and related functionality")

# load the libraries
library(BestModel)

PrepareNumericTitanic <- function() {
  titanic <- read.csv("~/BestModel/tests/testthat/titanic.csv")
  titanic <- titanic[, c("Survived", "Age",
                         "Siblings.Spouses.Aboard", "Parents.Children.Aboard", "Fare")]
  titanic$Survived = as.factor(titanic$Survived)
  levels(titanic$Survived) <- c("died", "survived")
  return(titanic)
}

test_that("Dataset predictions on Majority Vote", {
  # prepare the dataset
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- getModelComparisons(titanic[, -1], titanic[, 1])
  # predict the values for the dataset
  pred <- predict(comp, titanic[, -1])
  pred_list <- MajorityVote(pred)
})

test_that("Dataset predictions on Majority Weight", {
  # prepare the dataset
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- getModelComparisons(titanic[, -1], titanic[, 1])
  # predict the values for the dataset
  pred <- predict(comp, titanic[, -1])
  # turn pred list of DF's into a real list of values
  pred <- StripPredictions(pred)
  pred_list <- MajorityWeight(pred, runif(n=length(comp$model_list), min=0, max=1))
})

test_that("Dataset predictions on Average Vote", {
  # prepare the dataset
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- getModelComparisons(titanic[, -1], titanic[, 1])
  # predict the values for the dataset
  pred <- predict(comp, titanic[, -1])
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

