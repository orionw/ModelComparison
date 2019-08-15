context("Validation Types")

# load the libraries
library(ModelComparison)

PrepareNumericTitanic <- function() {

  print("Listing dirs")
  print(list.files(getwd()))
  print(list.files("ModelComparison"))
  print(getwd())
  titanic <- read.csv("titanic.csv")
  titanic <- titanic[, c("Survived", "Age",
                         "Siblings.Spouses.Aboard", "Parents.Children.Aboard", "Fare")]
  titanic$Survived = as.factor(titanic$Survived)
  levels(titanic$Survived) <- c("died", "survived")
  return(titanic)
}

test_that("80/20 validation works", {
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1], validation="80/20")
  # predict the values for the dataset
  pred.list <- predict(comp, titanic[, -1])
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model.list), length(pred.list))
  # ensure plot doesn't have any errors
  plot(comp, titanic[, 1], titanic[, -1])
})

test_that("50/50 validation works", {
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1], validation="50/50")
  # predict the values for the dataset
  pred.list <- predict(comp, titanic[, -1])
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model.list), length(pred.list))
  # ensure plot doesn't have any errors
  plot(comp, titanic[, 1], titanic[, -1])
})


test_that("95/05 validation works", {
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1], validation="95/05")
  # predict the values for the dataset
  pred.list <- predict(comp, titanic[, -1])
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model.list), length(pred.list))
  # ensure plot doesn't have any errors
  plot(comp, titanic[, 1], titanic[, -1])
})

test_that("CV validation works", {
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1], validation="cv")
  # predict the values for the dataset
  pred.list <- predict(comp, titanic[, -1])
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model.list), length(pred.list))
  # ensure plot doesn't have any errors
  plot(comp, titanic[, 1], titanic[, -1])
})
