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
  mlist <- list(ensem)
  names(mlist) <- c("Ensemble")
  comp <- convertToComparison(mlist, F)
  expect_equal(class(comp), "ModelComparison")
  # make sure plot works
  plot(comp, iris_ready[,1:4], iris_ready[, 5])
})
