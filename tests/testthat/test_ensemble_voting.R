context("Voting with Ensemble Functionality")

# load the libraries
library(BestModel)

# helper function for a 2 response iris dataset
PrepareNumericTitanic <- function() {
  titanic <- read.csv("~/BestModel/tests/testthat/titanic.csv")
  titanic <- titanic[, c("Survived", "Age",
                         "Siblings.Spouses.Aboard", "Parents.Children.Aboard", "Fare")]
  titanic$Survived = as.factor(titanic$Survived)
  levels(titanic$Survived) <- c("died", "survived")
  return(titanic)
}

test_that("Ensemble weighting function with auto-weighting", {
  # prepare the dataset
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1])
  ensem <- Ensemble(comp$model.list, "majorityWeight", titanic[, -1], titanic[, 1])
  expect_equal(length(ensem$weight.list), length(ensem$models))
  expect_equal(class(ensem), "Ensemble")
  pred <- predict(ensem, titanic[, -1])
  expect_equal(length(titanic[, 1]), length(pred))
})

test_that("Ensemble weighting function with given weights", {
  # prepare the dataset
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1])
  ensem <- Ensemble(comp$model.list, "majorityWeight", runif(n=length(comp$model.list), min=0, max=1))
  expect_equal(class(ensem), "Ensemble")
  pred <- predict(ensem, titanic[, -1])
  expect_equal(length(titanic[, 1]), length(pred))
})

test_that("Average weighting Ensemble", {
  # prepare the dataset
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1])
  ensem <- Ensemble(comp$model.list, "averageVote", runif(n=length(comp$model.list), min=0, max=1))
  expect_equal(class(ensem), "Ensemble")
  pred <- predict(ensem, titanic[, -1])
  expect_equal(length(titanic[, 1]), length(pred))
})

test_that("Majority Vote Ensemble", {
  # prepare the dataset
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1])
  ensem <- Ensemble(comp$model.list, "majorityVote", runif(n=length(comp$model.list), min=0, max=1))
  expect_equal(class(ensem), "Ensemble")
  pred <- predict(ensem, titanic[, -1])
  expect_equal(length(titanic[, 1]), length(pred))
})

test_that("Majority Vote Ensemble With only 2 models", {
  # prepare the dataset
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1], model.list = "all")
  models <- comp$model.list[c(F, T, T, F, F, F)]
  ensem <- Ensemble(models, "majorityVote")
  expect_equal(class(ensem), "Ensemble")
  pred <- predict(ensem, titanic[, -1])
  expect_equal(length(titanic[, 1]), length(pred))
})


