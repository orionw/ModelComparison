#' @export
getModelComparisons <-function(trainingSet,trainingClasses, testSet=NULL, modelList=NULL, dataSetSize="small") {
  print("In Function")
  trctrl <- caret::trainControl(method = "cv", savePredictions = T)
  set.seed(1)
  print(str(trainingSet))
  svm_linear <- caret::train(trainingSet, trainingClasses, method = "svmLinear",
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneLength = 10)
  return(svm_linear)
}
