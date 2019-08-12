context("Other package integration")

# load the libraries
library(ModelComparison)


PrepareNumericTitanic <- function() {
  titanic <- read.csv("~/ModelComparison/tests/testthat/titanic.csv")
  titanic <- titanic[, c("Survived", "Age",
                         "Siblings.Spouses.Aboard", "Parents.Children.Aboard", "Fare")]
  titanic$Survived = as.factor(titanic$Survived)
  levels(titanic$Survived) <- c("died", "survived")
  return(titanic)
}

test_that("Other packages used for ModelComparison", {
  # prepare the dataset
  titanic <- PrepareNumericTitanic()
  titanic$Survived
  # create the models
  library(e1071)
  naive.bayes = e1071::naiveBayes(as.factor(Survived)~., threshold = 0.001, eps = 5,
                                  data=titanic)
  library(randomForest)
  rforest = randomForest(formula = as.factor(Survived)~., data = titanic, type=raw,
                         cutoff=c(.45, .55))
  library(nnet)
  capture.output(neural.net <- nnet(as.factor(Survived) ~ ., data=titanic,
                                    size=10,decay=5e-2, maxit=50))
  library(glmnet)
  matrix.glmnet <- data.matrix(titanic[, -1])
  glmnet <- glmnet(x=matrix.glmnet, y=factor(titanic$Survived), family = "binomial", lambda=.02)

  svm.radial = svm(as.factor(Survived)~., data = titanic, kernel = "radial", probability=TRUE)


  models <- list(naive.bayes, rforest, neural.net, glmnet, svm.radial)
  names(models) <- c("naive.bayes", "rforest", "neural.net", "glmnet", "svm.radial")

  comp <- ModelComparison(models, F)
  pred.basic <- predict(comp, titanic[, -1])
  # all metrics default
  plot(comp, titanic[, 1], titanic[, -1], plot.type="all")
  # roc curve works
  plot(comp, titanic[, 1], titanic[, -1], plot.type="roc")
  # multiple metrics, two word metrics, uncapitalized
  plot(comp, titanic[, 1], titanic[, -1], plot.type=list("precision", "accuracy",
                                               "recall", "detection rate"))
  # verify we can  make an Ensemble
  ensem <- Ensemble(comp$model.list, "majorityWeight", titanic[, -1], titanic[, 1])
  expect_equal(length(ensem$weight.list), length(models))

  pred.ensem <- predict(ensem, titanic[, -1])
  expect_equal(length(titanic[, 1]), length(pred.ensem))

})
