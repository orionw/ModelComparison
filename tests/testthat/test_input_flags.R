context("Input ModelComparison Functionality")

# load the libraries
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

test_that("Model Building Flags Work", {
  fast <- GetBuildFlags(list("fast", "SVMRadial"))
  print(fast)
  expect_equal(fast, c(T, F, F, F, T, F, F, F))
})
