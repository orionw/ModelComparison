context("Stacking Helper Functions")

# load the libraries
library(BestModel)

prepare_iris <- function() {
  data(iris)
  toBeRemoved<-which(iris$Species=="setosa")
  irisReal <-iris[-toBeRemoved,]
  irisReal <- droplevels(irisReal)
  levels(irisReal$Species) <- c('versicolor', 'virginica' )
  return(irisReal)
}

test_that("Add predictions to df for training with stacking", {
  # helper function for a 2 response iris dataset
    iris_ready <- prepare_iris()
    x.values = iris_ready[, 1:4]
    dim.x <- dim(x.values)
    # create the models
    comp <- getModelComparisons(x.values, iris_ready[,5])
    # get expected values
    dim.exp.x <- dim.x
    dim.exp.x[[2]] = dim.exp.x[[2]] + length(comp$model_list)
    # get expected names
    expected.names <- c(as.character(colnames(x.values)), as.character(names(comp$model_list)))
    # get the new dataframe
    df.for.stacking <- BestModel::GetPredictionsForStacking(comp$model_list, x.values)
    expect_equal(dim(df.for.stacking), dim.exp.x)
    expect_equal(names(df.for.stacking), expected.names)
})
