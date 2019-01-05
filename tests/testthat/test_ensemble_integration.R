context("Ensemble - Model Comparison Integration")

# load the libraries
library(BestModel)

# test_that("Ensemble plot", {
#   # TODO: decide what to do with this
# })

test_that("Ensemble used in a ModelComparison", {
  # prepare the dataset
  iris <- PrepareIris()
  # create the models
  comp <- GetModelComparisons(iris[,1:4], iris[,5])
  ensem1 <- Ensemble(comp$model.list, "majorityWeight", iris[,1:4], iris[,5])
  ensem2 <- Ensemble(comp$model.list, "majorityVote")
  ensem3 <- Ensemble(comp$model.list, "averageVote")
  # put them in a named list for the comparison
  mlist <- list(ensem1, ensem2, ensem3)
  names(mlist) <- c("Ensemble1", "Ensemble2", "Ensemble3")
  comp <- ModelComparison(mlist, F)
  expect_equal(class(comp), "ModelComparison")
  # make sure plot works
  print(plot(comp, iris[, 5], iris[,1:4]))
})
