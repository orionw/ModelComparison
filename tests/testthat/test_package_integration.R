context("Other package integration")

# load the libraries
library(BestModel)


PrepareNumericTitanic <- function() {
  titanic <- read.csv("~/BestModel/tests/testthat/titanic.csv")
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
  naive.bayes = e1071::naiveBayes(as.factor(Survived)~., threshold = 0.001, eps = 5, data=titanic)
  library(randomForest)
  rforest = randomForest(formula = as.factor(Survived)~., data = titanic, type=raw, cutoff=c(.45, .55))
  library(nnet)
  capture.output(neural.net <- nnet(as.factor(Survived) ~ ., data=titanic,
                                    size=10,decay=5e-2, maxit=50))
  library(glmnet)
  matrix.glmnet <- data.matrix(titanic[, -1])
  glmnet <- glmnet(x=matrix.glmnet, y=factor(titanic$Survived), family = "binomial", lambda=.02)

  models <- list(naive.bayes, rforest, neural.net, glmnet)
  names(models) <- c("naive.bayes", "rforest", "neural.net", "glmnet")

  comp <- ModelComparison(models, F)
  # all metrics default
  plot(comp, titanic[, 1], titanic[, -1], "all")
  # roc curve works
  plot(comp, titanic[, 1], titanic[, -1], "roc")
  # multiple metrics, two word metrics, uncapitalized
  plot(comp, titanic[, 1], titanic[, -1], list("auc", "precision", "accuracy",
                                               "recall", "detection rate"))


  # ensem1 <- Ensemble(comp$model.list, "majorityWeight", iris[,1:4], iris[,5])

})
