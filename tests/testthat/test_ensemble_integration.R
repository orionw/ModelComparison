context("Ensemble - Model Comparison Integration")

# load the libraries
library(BestModel)

# helper function for a 2 response iris dataset
prepare_iris <- function() {
  data(iris)
  toBeRemoved<-which(iris$Species=="setosa")
  irisReal <-iris[-toBeRemoved,]
  irisReal <- droplevels(irisReal)
  levels(irisReal$Species) <- c('versicolor', 'virginica' )
  return(irisReal)
}

test_that("Ensemble plot", {
  # TODO: decide what to do with this
})

test_that("Ensemble used in a ModelComparison", {
  # prepare the dataset
  iris_ready <- prepare_iris()
  # create the models
  comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5])
  ensem <- Ensemble(comp$model_list, "majorityWeight", iris_ready[,1:4], iris_ready[,5])
  comp <- convertToComparison(list(ensem), F)
  plot(comp, iris_ready[,1:4], iris_ready[, 5])
})
