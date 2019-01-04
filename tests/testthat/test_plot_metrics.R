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
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1], model.list = "all")
  # predict the values for the dataset
  pred.list <- predict(comp, titanic[, -1])
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model.list), length(pred.list))
  # ensure plot doesn't have any errors
  plot(comp, titanic[, 1], titanic[, -1], plot.type=c("Accuracy"))
})

test_that("ROC Plot works", {
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1], model.list = "all")
  # predict the values for the dataset
  pred.list <- predict(comp, titanic[, -1])
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model.list), length(pred.list))
  # ensure plot doesn't have any errors
  plot(comp, titanic[, 1], titanic[, -1], plot.type=c("ROC"))
  # check AUC
  plot(comp, titanic[, 1], titanic[, -1], plot.type=c("AUC"))
})

test_that("Other metric plots work", {
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1], model.list = "all")
  # predict the values for the dataset
  pred.list <- predict(comp, titanic[, -1])
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model.list), length(pred.list))
  # ensure plot doesn't have any errors
  plot(comp, titanic[, 1], titanic[, -1], plot.type="Precision")
  plot(comp, titanic[, 1], titanic[, -1], plot.type=c("Specificity"))
  plot(comp, titanic[, 1], titanic[, -1], plot.type="Recall")
  plot(comp, titanic[, 1], titanic[, -1], plot.type=c("Detection Rate"))
})

test_that("Error handling on Plotting", {
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1], model.list = "all")
  # predict the values for the dataset
  pred.list <- predict(comp, titanic[, -1])
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model.list), length(pred.list))
  expect_error(plot(comp, titanic[, 1], titanic[, -1], plot.type=c("blah")),
               "plot.type is not a valid metric name. Please see the documentation")
  expect_error(plot(comp, titanic[, 1], titanic[, -1], plot.type="not a list"),
               "plot.type is not a valid metric name. Please see the documentation")
  expect_error(plot(comp, titanic[, 1], titanic[, -1], plot.type=3),
               "Undefined plot.type.  Please check the documentation.")
  # ROC with others, fails
  expect_error(plot(comp, titanic[, 1], titanic[, -1], plot.type=c("ROC, Specificity",
                                                                   "Precision", "AUC",
                                                                   "Recall", "Detection Rate")),
               "plot.type is not a valid metric name. Please see the documentation for details")
})


test_that("Plot All", {
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1], model.list = "all")
  # predict the values for the dataset
  pred.list <- predict(comp, titanic[, -1])
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model.list), length(pred.list))
  plot(comp, titanic[, 1], titanic[, -1], plot.type=c("All"))
  # without vector works
  plot(comp, titanic[, 1], titanic[, -1], plot.type="All")
  # manual types work
  plot(comp, titanic[, 1], titanic[, -1], plot.type=c("Specificity", "Precision", "AUC",
                                                      "Recall", "Detection Rate"))
})

