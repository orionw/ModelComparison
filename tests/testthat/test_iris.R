context("Basic Full Functionality Binary")

library(plyr)
library(ggplot2)
library(BestModel)
data(iris)

prepare_iris <- function() {
  toBeRemoved<-which(iris$Species=="setosa")
  irisReal <-iris[-toBeRemoved,]
  irisReal <- droplevels(irisReal)
  levels(irisReal$Species) <- c('versicolor', 'virginica' )
  return(irisReal)
}

test_that("retured object has the correct structure", {
  iris_ready <- prepare_iris()
  comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5])
  expect_equal(class(comp), "ModelComparison")
  expect_equal(class(comp$model_list), "list")
  expect_equal(comp$.multi_class, FALSE)
  # check individual models
})


test_that("Object can predict and display ROC curves", {
  iris_ready <- prepare_iris()
  comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5])
  #pred_list <- predict(comp, iris_ready)
  #assertthat::are_equal(length(pred_list[0]), iris_ready$Species)
  print("Now to the predictions")
  #print(pred_list)
  print(length(iris_ready[,5]))
  print(dim(iris_ready[,1:4]))
  #print(comp)
  plot(comp, iris_ready[,5], iris_ready[,1:4])

})

#
# a$svmLinear
# a$neuralNet$neuralNet$finalModel

