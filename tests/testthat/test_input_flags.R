context("Input ModelComparison Functionality")

# load the libraries
library(ModelComparison)

## Ouput is ordered in:
#  glm glmnet randomforest knn svmradial svmlinear neuralnet
test_that("Model Building Flags Work", {
  # name and order of the results for model building
  modelNames <- c("glm", "glmnet", "randomforest", "knn", "svmradial", "svmlinear", "neuralnet")

  fast <- GetBuildFlags(list("fast", "SVMRadial"))
  expected.fast <- c(F, T, F, F, T, T, F)
  # set names so that the vectors are equal
  names(expected.fast) <- modelNames
  expect_equal(fast, expected.fast)

  # test all option and capitalization
  all <- GetBuildFlags(list("ALL", "NeuralNet"))
  expect.all <- c(T, T, T, T, T, T, T)
  names(expect.all) <- modelNames
  expect_equal(all, expect.all)

  # test the expensive option
  expensive <- GetBuildFlags(list("expensive", "KNN"))
  expect.expensive <- c(F, T, T, T, T, F, T)
  names(expect.expensive) <- modelNames
  expect_equal(expensive, expect.expensive)

  # Test them all individually
  all.names <- list("glm", "glmnet", "randomforest", "knn", "svmradial", "svmlinear", "neuralnet")
  individual <- GetBuildFlags(all.names)
  expect_equal(all, expect.all)
})

