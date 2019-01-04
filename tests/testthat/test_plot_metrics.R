context("Metrics Plotting")

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

test_that("Accuracy Plot works", {
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- getModelComparisons(titanic[, -1], titanic[, 1], modelList = "all")
  # predict the values for the dataset
  pred_list <- predict(comp, titanic[, -1])
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model_list), length(pred_list))
  # ensure plot doesn't have any errors
  plot(comp, titanic[, 1], titanic[, -1], plot.type="Accuracy")
})

test_that("ROC Plot works", {
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- getModelComparisons(titanic[, -1], titanic[, 1], modelList = "all")
  # predict the values for the dataset
  pred_list <- predict(comp, titanic[, -1])
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model_list), length(pred_list))
  # ensure plot doesn't have any errors
  plot(comp, titanic[, 1], titanic[, -1], plot.type="ROC")
})

test_that("Other metric plots work", {
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- getModelComparisons(titanic[, -1], titanic[, 1], modelList = "all")
  # predict the values for the dataset
  pred_list <- predict(comp, titanic[, -1])
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model_list), length(pred_list))
  # ensure plot doesn't have any errors
  plot(comp, titanic[, 1], titanic[, -1], plot.type="Precision")
  plot(comp, titanic[, 1], titanic[, -1], plot.type="Specificity")
  plot(comp, titanic[, 1], titanic[, -1], plot.type="Recall")
  plot(comp, titanic[, 1], titanic[, -1], plot.type="Detection Rate")
})

test_that("Plot with unknown metric name fails", {
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- getModelComparisons(titanic[, -1], titanic[, 1], modelList = "all")
  # predict the values for the dataset
  pred_list <- predict(comp, titanic[, -1])
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model_list), length(pred_list))
  expect_error(plot(comp, titanic[, 1], titanic[, -1], plot.type="blah"),
               "plot.type is not a valid metric name. Please see the documentation")
})

test_that("Plot with AUC", {
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- getModelComparisons(titanic[, -1], titanic[, 1], modelList = "all")
  # predict the values for the dataset
  pred_list <- predict(comp, titanic[, -1])
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model_list), length(pred_list))
  plot(comp, titanic[, 1], titanic[, -1], plot.type="AUC")
})

