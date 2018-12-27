context("Basic Full Functionality Binary")

# load the libraries
library(mlbench)
library(plyr)
library(ggplot2)
library(BestModel)
library(caret)

# helper function for a 2 response iris dataset
prepare_iris <- function() {
  data(iris)
  toBeRemoved<-which(iris$Species=="setosa")
  irisReal <-iris[-toBeRemoved,]
  irisReal <- droplevels(irisReal)
  levels(irisReal$Species) <- c('versicolor', 'virginica' )
  return(irisReal)
}

test_that("retured object has the correct structure", {
  # prepare the dataset
  iris_ready <- prepare_iris()
  # create the models
  comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5])
  # check that the class is correct
  expect_equal(class(comp), "ModelComparison")
  # check that it has the right classes for it's variables
  expect_equal(class(comp$model_list), "list")
  expect_equal(comp$.multi_class, FALSE)
  # check individual models
  # TODO
})


test_that("Basic Iris Dataset - Minimum Viable Product", {
  # prepare the dataset
  iris_ready <- prepare_iris()
  # create the models
  comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5])
  # predict the values for the dataset
  pred_list <- predict(comp, iris_ready[,1:4])
  # check that the lists are the same size
  for (item in pred_list) {
    # index into the list and then grab the first column
    expect_equal(print(length(item[[1]][,1])), length(iris_ready$Species))
  }
  # check that there are the same number of predictions as there are models
  expect_equal(length(comp$model_list), length(pred_list))
  # ensure plot doesn't have any errors
  plot(comp,iris_ready[,1:4], iris_ready[,5] )

})

test_that("MVP on more complex dataset", {
  # Johns Hopkins University Ionosphere database from MLBench
  data(Ionosphere)
  # remove Y variable
  x.val = Ionosphere[,-length(Ionosphere)]
  # remove that categorical variables
  x.val = x.val[,c(-1, -2)]
  # get comparisons
  ion <- getModelComparisons(x.val, Ionosphere$Class)
  plot(ion, x.val, Ionosphere$Class)
})

prepare_categorical_breast_cancer <- function() {
  data(BreastCancer)
  # remove NA rows
  BreastCancer <- na.omit(BreastCancer)
  BreastCancer = BreastCancer[complete.cases(BreastCancer), ]
  breast.cancer.y = BreastCancer$Class
  # No NA's
  assertthat::are_equal(0, sum(is.na(BreastCancer)))
  # remove the class predictions
  BreastCancer = BreastCancer[,-length(BreastCancer)]
  # Remove ID column
  BreastCancer = BreastCancer[,-1]
  return(list(BreastCancer, breast.cancer.y))
}

test_that("Categorical Data given in one hot encoding", {
  ###### Start by encoding in one hot #########
  df.list <- prepare_categorical_breast_cancer()
  breast.cancer.x = df.list[[1]]
  breast.cancer.y = df.list[[2]]
  # one hot encode the data
  dmy <- caret::dummyVars(" ~ .", data = breast.cancer.x)
  breast.cancer.x <- data.frame(predict(dmy, newdata = breast.cancer.x))

  #######  use BestModel on the categorical data  ######
  cancer <- getModelComparisons(breast.cancer.x, breast.cancer.y)
  plot(cancer, breast.cancer.x, breast.cancer.y)
})


test_that("Categorical Data not given in one hot encoding", {
  ## Give dataset not in one-hot encoding
  # give warning that we are attempting to convert
  df.list <- prepare_categorical_breast_cancer()
  breast.cancer.x = df.list[[1]]
  breast.cancer.y = df.list[[2]]

  #######  use BestModel on the categorical data  ######
  cancer <- getModelComparisons(breast.cancer.x, breast.cancer.y)
  plot(cancer, breast.cancer.x, breast.cancer.y)
})

#
# a$svmLinear
# a$neuralNet$neuralNet$finalModel

