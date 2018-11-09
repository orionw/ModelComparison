#' ModelComparisons()
ModelComparison <- function(ModelList) {
  # we can add our own integrity checks
  value <- list(svmLinear = ModelList["svmLinear"],
                neuralNet = ModelList["neuralNet"],
                glmnet = ModelList["glmnet"],
                randomForest = ModelList["randomForest"],
                glm = ModelList["glm"])

  # class can be set using class() or attr() function
  class(value) <- "ModelComparison"
  print(value)
  print(class(value))
  return(value)
}

getROCGraph <- function(modelList, labels) {
  i = 0
  for (model in modelList) {
  preds - predict(model, newdata = labels, type="raw")
  assign(paste("roc.",i), roc(labels, preds))
    i = i + 1
  }
  #Plot ROC curves side by side
  plot(roc.0, col = rainbow(0))
  for (count in i) {
    # adjust for the first one being outside the loop
    count = count + 1
    plot(paste("roc.", i), col = rainbow(i), add=T)
  }

  legend("bottomright",legend = c("Two-Factor (0.5825)","Full GLM (0.6841)", "SVM1 (0.7553)", "*SVM validate1 (0.7581)", "Neural Net (.8242)"), cex = .7,
         col = c("blue", "red", "green", "purple"), lty = c(1), ncol = 1, text.font = 4, box.lty = 0)

  return(recordPlot(load=NULL, attach=NULL))
}

#' This function evalutates many different machine learning models and returns those models with comparison charts
#' @param trainingSet the dataset to be trained on
#' @param trainingClasses the labels of the training set
#' @keywords
#' @export
#' @examples
#' getModelComparisons()
getModelComparisons <-function(trainingSet,trainingClasses, testSet=NULL, modelList=NULL, dataSetSize="small") {
  print("In Function")
  trctrl <- caret::trainControl(method = "cv", savePredictions = T)
  set.seed(1)
  svmLinear <- caret::train(trainingSet, trainingClasses, method = "svmLinear",
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneLength = 10)

  neuralNet <- caret::train(trainingSet, trainingClasses, method = "nnet",
                             trControl=trctrl,
                             preProcess = c("center", "scale"),
                             tuneLength = 10)

  glmnet <- caret::train(trainingSet, trainingClasses, method = "glmnet",
                            trControl=trctrl,
                            preProcess = c("center", "scale"),
                            tuneLength = 10)

  randomForest <- caret::train(trainingSet, trainingClasses, method = "rf",
                            trControl=trctrl,
                            preProcess = c("center", "scale"),
                            tuneLength = 10)

  glm_model <- caret::train(trainingSet, trainingClasses, method = "glm",
                            trControl=trctrl,
                            preProcess = c("center", "scale"),
                            tuneLength = 10)

  print("Done PRocessing")
  modelVec = list(svmLinear, neuralNet, glmnet, randomForest, glm_model)
  visualizeROC <- getROCGraph(modelVec, trainingClasses)
  names(modelVec) <- c("svmLinear", "neuralNet", "glmnet", "randomForest", "glm", "compareROC")
  modelComp <- ModelComparison(modelVec, visualizeROC)
  return(modelComp)
}
