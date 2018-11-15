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

getROCGraph <- function(modelList, test_data, multi_class, labels) {
  i = 0
  for (model in modelList) {
    if (!is.null(model)) {
      print(model)
      preds <- predict(model, newdata = test_data, type="raw")
      print("Pred done")
      if (multi_class) {
        print(length(as.numeric(preds)))
        print(dim(test_data))
        print(length(labels))
        assign(paste("roc.",i, sep=""), pROC::multiclass.roc(labels, as.numeric(preds)))
        print(paste("roc.",i, sep=""))

      } else {
        assign(paste("roc.",i, sep=""), pROC::roc(test_data, preds))
        print(paste("roc.",i), sep = "")
      }
    i = i + 1
    }
  }
  # deal with this later.  Quick fix to resolve i
  i = i - 1
  print("Assigned")
  #Plot ROC curves side by side
  if (multi_class) {
    rs <- roc.0[['rocs']]
    pROC::plot.roc(rs[[1]])
    sapply(2:length(rs),function(i) pROC::lines.roc(rs[[i]],col=i))
    #plot(roc.0, col = rainbow(0), add=T)
    #browser()
    p <- recordPlot()
    plot.new() ## clean up device
    p # redraw
    print("plotted first")
    for (count in i) {
      # adjust for the first one being outside the loop
      count = count + 1
      rs <- get(paste("roc.", i, sep=""))[['rocs']]
      pROC::plot.roc(rs[[1]])
      sapply(1:length(rs),function(i) pROC::lines.roc(rs[[i]],col=i))
    }
  } else {
    # Do regular ROC here
  }
  print("finished plottting")

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
getModelComparisons <-function(trainingSet,training_classes_input, validation="80/20", modelList=NULL, dataSetSize="small") {
  print("In Function")

  # prep the data if not already a factor
  training_classes_input = as.factor(training_classes_input)
  set.seed(sample(1:9999999, 1))
  multi_class = levels(training_classes_input) > 2

  # Get the method of validation and prepare testing and training sets
  if (validation == "80/20") {
    # partition the data into training and testing data stratified by class
    trainIndex <- caret::createDataPartition(training_classes_input, p=0.8, list=F)
    # get the dataframes
    training_data <- trainingSet[trainIndex,]
    testing_data <- trainingSet[-trainIndex,]
    # get the labels
    training_classes <- training_classes_input[trainIndex]
    testing_classes <- training_classes_input[-trainIndex]
    # we will take care of the validation
    trctrl <- caret::trainControl(method = "none", savePredictions = T)
  } else {
    # we don't need a specific testing set
    training_data = trainingSet
    testing_data <- trainingSet
    testing_classes <- training_classes
    if (validation == "cv") {
      trctrl <- caret::trainControl(method = "cv", savePredictions = T)
    } else {
      trctrl <- caret::trainControl(method = "none", savePredictions = T)
    }
  }

  print("validation method complete")
  tune_length = 1

  svmLinear <- caret::train(training_data, training_classes, method = "svmLinear",
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneLength = tune_length)

  neuralNet <- caret::train(training_data, training_classes, method = "nnet",
                             trControl=trctrl,
                             preProcess = c("center", "scale"),
                             tuneLength = tune_length)

  glmnet <- caret::train(training_data, as.factor(training_classes), method = "glmnet",
                            trControl=trctrl,
                            preProcess = c("center", "scale"),
                            tuneLength = tune_length)


  randomForest <- caret::train(training_data, training_classes, method = "rf",
                            trControl=trctrl,
                            preProcess = c("center", "scale"),
                            tuneLength = tune_length)

  if (!multi_class) {

  glm_model <- caret::train(training_data, as.factor(training_classes), method = "glm",
                            trControl=trctrl,
                            preProcess = c("center", "scale"),
                            tuneLength = tune_length)
  } else {
    glm_model = NULL
  }

  print("Done Processing")

  # get the visuals for all the models
  modelVec = list(svmLinear, neuralNet, glmnet, randomForest, glm_model)
  visualizeROC <- getROCGraph(modelVec, test_data = testing_data, multi_class = multi_class,labels=testing_classes)
  names(modelVec) <- c("svmLinear", "neuralNet", "glmnet", "randomForest", "glm", "compareROC")
  modelComp <- ModelComparison(modelVec, visualizeROC)
  return(modelComp)
}