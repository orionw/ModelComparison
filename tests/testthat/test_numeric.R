context("Numeric Dataset Functionality")

# load the libraries
library(mlbench)
library(plyr)
library(ggplot2)
library(BestModel)
library(caret)

PrepareNumericTitanic <- function() {
  titanic <- read.csv("~/BestModel/tests/testthat/titanic.csv")
  titanic <- titanic[, c("Survived", "Age",
                            "Siblings.Spouses.Aboard", "Parents.Children.Aboard", "Fare")]
  titanic$Survived = as.factor(titanic$Survived)
  levels(titanic$Survived) <- c("died", "survived")
  return(titanic)
}

test_that("Retured object has the correct structure", {
  # prepare the dataset
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1])
  # check that the class is correct
  expect_equal(class(comp), "ModelComparison")
  # check that it has the right classes for it's variables
  expect_equal(class(comp$model.list), "list")
  expect_equal(comp$.multi.class, FALSE)
  # check individual models
  # TODO
})

test_that("Titanic Dataset - Minimum Viable Product", {
  # prepare the dataset
  titanic <- PrepareNumericTitanic()
  # create the models
  comp <- GetModelComparisons(titanic[, -1], titanic[, 1])
  # predict the values for the dataset
  pred.list <- predict(comp, titanic[, -1])
  # check that the lists are the same size
  for (item in pred.list) {
    # index into the list and then grab the first column
    expect_equal((length(item[,1])), length(titanic$Survived))
  }
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model.list), length(pred.list))
  # ensure plot doesn't have any errors
  plot(comp, titanic[, 1], titanic[, -1])

})

test_that("MVP on more complex dataset", {
  # Johns Hopkins University Ionosphere database from MLBench
  data(Ionosphere)
  # remove Y variable
  x.val = Ionosphere[,-length(Ionosphere)]
  # remove that categorical variables
  x.val = x.val[,c(-1, -2)]
  # get comparisons - warning occurs from too easy of data so suppress it for testing purposes
  ion <- GetModelComparisons(x.val, Ionosphere$Class)
  plot(ion, Ionosphere$Class, x.val)
})


# test_that("Convert a list of models and plot it", {
#   # prepare the dataset
#   iris_ready <- prepare_iris()
#   # create the models
#   comp <- GetModelComparisons(iris_ready[,1:4], iris_ready[,5], model.list="fast")
#   # make a list of models and mix it up
#   different_list = comp$model.list
#   names(different_list) <- c("Random", "Stuff")
#   different_list = rev(different_list)
#   # plot list of models and see if the conversion works
#   comp_model = ModelComparison(different_list, multi_class = F)
#   plot(comp_model, iris_ready[,1:4], iris_ready[,5] )
# })
