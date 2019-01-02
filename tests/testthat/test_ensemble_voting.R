context("Voting with Ensemble Functionality")

# load the libraries
library(BestModel)

# # helper function for a 2 response iris dataset
# prepare_iris <- function() {
#   data(iris)
#   toBeRemoved<-which(iris$Species=="setosa")
#   irisReal <-iris[-toBeRemoved,]
#   irisReal <- droplevels(irisReal)
#   levels(irisReal$Species) <- c('versicolor', 'virginica' )
#   return(irisReal)
# }
#
# test_that("Ensemble weighting function with auto-weighting", {
#   # prepare the dataset
#   iris_ready <- prepare_iris()
#   # create the models
#   comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5])
#   ensem <- Ensemble(comp$model_list, "majorityWeight", iris_ready[,1:4], iris_ready[,5])
#   expect_equal(length(ensem$weight.list), length(ensem$models))
#   expect_equal(class(ensem), "Ensemble")
#   pred <- predict(ensem, iris_ready[, 1:4])
#   expect_equal(length(iris_ready[, 5]), length(pred))
# })
#
# test_that("Ensemble weighting function with given weights", {
#   # prepare the dataset
#   iris_ready <- prepare_iris()
#   # create the models
#   comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5])
#   ensem <- Ensemble(comp$model_list, "majorityWeight", runif(n=length(comp$model_list), min=0, max=1))
#   expect_equal(class(ensem), "Ensemble")
#   pred <- predict(ensem, iris_ready[, 1:4])
#   expect_equal(length(iris_ready[, 5]), length(pred))
# })
#
# test_that("Average weighting Ensemble", {
#   # prepare the dataset
#   iris_ready <- prepare_iris()
#   # create the models
#   comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5])
#   ensem <- Ensemble(comp$model_list, "averageVote")
#   expect_equal(class(ensem), "Ensemble")
#   pred <- predict(ensem, iris_ready[, 1:4])
#   expect_equal(length(iris_ready[, 5]), length(pred))
# })
#
# test_that("Majority Vote Ensemble", {
#   # prepare the dataset
#   iris_ready <- prepare_iris()
#   # create the models
#   comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5])
#   ensem <- Ensemble(comp$model_list, "majorityVote")
#   expect_equal(class(ensem), "Ensemble")
#   pred <- predict(ensem, iris_ready[, 1:4])
#   expect_equal(length(iris_ready[, 5]), length(pred))
# })
#
# test_that("Majority Vote Ensemble With only 2 models", {
#   # prepare the dataset
#   iris_ready <- prepare_iris()
#   # create the models
#   comp <- getModelComparisons(iris_ready[,1:4], iris_ready[,5], modelList = "all")
#   models <- comp$model_list[c(F, F, T, T, F)]
#   ensem <- Ensemble(models, "majorityVote")
#   expect_equal(class(ensem), "Ensemble")
#   pred <- predict(ensem, iris_ready[, 1:4])
#   expect_equal(length(iris_ready[, 5]), length(pred))
# })


