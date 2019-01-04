context("Stacking Helper Functions")

# load the libraries
library(BestModel)

test_that("Add predictions to df for training with stacking", {
  # helper function for a 2 response iris dataset
    iris <- PrepareIris()
    x.values = iris[, 1:4]
    dim.x <- dim(x.values)
    # create the models
    comp <- GetModelComparisons(x.values, iris[,5])
    # get expected values
    dim.exp.x <- dim.x
    dim.exp.x[[2]] = dim.exp.x[[2]] + length(comp$model.list)
    # get expected names
    expected.names <- c(as.character(colnames(x.values)), as.character(names(comp$model.list)))
    # get the new dataframe
    df.for.stacking <- BestModel::GetPredictionsForStacking(comp$model.list, x.values)
    expect_equal(dim(df.for.stacking), dim.exp.x)
    expect_equal(names(df.for.stacking), expected.names)
})
