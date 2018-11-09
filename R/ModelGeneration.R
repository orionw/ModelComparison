#' ModelComparisons()
ModelComparison <- function(ModelList) {
  # we can add our own integrity checks
  value <- list(svmLinear = ModelList["svmLinear"], neuralNet = ModelList["neuralNet"])
  # class can be set using class() or attr() function
  class(value) <- "ModelComparison"
  print(value)
  print(class(value))
  return(value)
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
  print("Done PRocessing")
  modelVec = list(svmLinear, neuralNet)
  names(modelVec) <- c("svmLinear", "neuralNet")
  modelComp <- ModelComparison(modelVec)
  return(modelComp)
}
