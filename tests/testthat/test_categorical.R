context("Catagorical Dataset Functionality")

# load the libraries
library(mlbench)
library(plyr)
library(ggplot2)
library(BestModel)
library(caret)

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

test_that("Categorical Data IS given in one hot encoding", {
  ###### Start by encoding in one hot #########
  df.list <- prepare_categorical_breast_cancer()
  breast.cancer.x = df.list[[1]]
  breast.cancer.y = df.list[[2]]
  # one hot encode the data
  dmy <- caret::dummyVars(" ~ .", data = breast.cancer.x)
  breast.cancer.x <- data.frame(predict(dmy, newdata = breast.cancer.x))

  #######  use BestModel on the categorical data  ######
  cancer <- getModelComparisons(breast.cancer.x, breast.cancer.y)
  expect_equal(cancer$force_prepared, FALSE)
  plot(cancer, breast.cancer.x, breast.cancer.y)
})


test_that("Categorical Data NOT given in one hot encoding", {
  ## Give dataset not in one-hot encoding
  # give warning that we are attempting to convert
  df.list <- prepare_categorical_breast_cancer()
  breast.cancer.x = df.list[[1]]
  breast.cancer.y = df.list[[2]]

  #######  use BestModel on the categorical data  ######
  # Note that both will check if the data is encoded properly
  cancer <- getModelComparisons(breast.cancer.x, breast.cancer.y)
  expect_equal(cancer$force_prepared, TRUE)
  plot(cancer, breast.cancer.x, breast.cancer.y)
})

test_that("Categorical Data HALF given in one hot encoding", {
  ## Give dataset not in one-hot encoding
  # give warning that we are attempting to convert
  df.list <- prepare_categorical_breast_cancer()
  breast.cancer.x = df.list[[1]]
  breast.cancer.y = df.list[[2]]

  #######  use BestModel on the categorical data  ######
  # data is not encoded
  cancer <- getModelComparisons(breast.cancer.x, breast.cancer.y)
  expect_equal(cancer$force_prepared, TRUE)

  # one hot encode the data
  dmy <- caret::dummyVars(" ~ .", data = breast.cancer.x)
  breast.cancer.x <- data.frame(predict(dmy, newdata = breast.cancer.x))
  # data is encoded
  plot(cancer, breast.cancer.x, breast.cancer.y)
})

test_that("Categorical Data other HALF given in one hot encoding", {
  ## Give dataset not in one-hot encoding
  # give warning that we are attempting to convert
  df.list <- prepare_categorical_breast_cancer()
  breast.cancer.x = df.list[[1]]
  breast.cancer.y = df.list[[2]]
  breast.cancer.x1 = breast.cancer.x
  # one hot encode the data
  dmy <- caret::dummyVars(" ~ .", data = breast.cancer.x)
  breast.cancer.x <- data.frame(predict(dmy, newdata = breast.cancer.x))

  #######  use BestModel on the categorical data  ######
  # data is encoded
  cancer <- getModelComparisons(breast.cancer.x, breast.cancer.y)
  expect_equal(cancer$force_prepared, FALSE)
  # data is not encoded
  plot(cancer, breast.cancer.x1, breast.cancer.y)
})
