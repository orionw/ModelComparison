#' ModelComparisons()
ModelComparison <- function(ModelList, multi_class) {
  # we can add our own integrity checks
  comparison <- "Model Comparison Object"
  model_list <- list(svmLinear = ModelList["svmLinear"],
                neuralNet = ModelList["neuralNet"],
                glmnet = ModelList["glmnet"],
                randomForest = ModelList["randomForest"],
                glm = ModelList["glm"])

  # class can be set using class() or attr() function
  class(comparison) <- "ModelComparison"
  comparison$model_list <- model_list
  comparison$.multiclass <- multi_class
  print(comparison)
  print(class(comparison))
  return(comparison)
}

#' This function evalutates many different machine learning models and returns those models with comparison charts
#' @param trainingSet the dataset to be trained on
#' @param trainingClasses the labels of the training set
#' @keywords
#' @export
#' @examples
#' plot()
plot.ModelComparison <- function(object, labels, predictions_list) {
  if (object$.multiclass) {
    # do stuff later
  } else {
    for (model in object.modelList) {
      if (!is.null(model)) {
        if (i == 1) {
          # do this to init the plot
          roc_plot <- pROC::roc(labels, predictions_list[[i]])
          plot(roc_plot, col = "red")
        } else {
          roc_plot <- pROC::roc(labels, predictions_list[[i]])
          plot(roc_plot, add = T, col = "purple")
        }
      }
    }
  }
}

#' This function predict on many different machine learning models
#' @param trainingSet the dataset to be trained on
#' @param trainingClasses the labels of the training set
#' @keywords
#' @export
#' @examples
#' predict()
predict.ModelComparison <- function(object, new_data) {
  predictions_list = list()
  i = 0
  if (object$.multi_class) {
    # do something TODO
    for (model in object.modelList) {
      i = i + 1
    }
  } else {
    for (model in object.modelList) {
      i = i + 1
      if (!is.null(model)) {
        predictions_list[[i]] <- predict(model, newdata = new_data, type="raw")
      }
    }
  }
  return(predictions_list)
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
  multi_class = (nlevels(training_classes_input) > 2)

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
  names(modelVec) <- c("svmLinear", "neuralNet", "glmnet", "randomForest", "glm")
  modelComp <- ModelComparison(modelVec, multi_class)
  return(modelComp)

}




# getROCGraph <- function(modelList, test_data,  labels, multi_class) {
#   i = 1
#   for (model in modelList) {
#     if (!is.null(model)) {
#       print(model)
#       print(dim(test_data))
#       preds <- predict(model, newdata = test_data, type="raw")
#       print(length(preds))
#       print("Pred done")
#       if (multi_class) {
#         assign(paste("roc.",i, sep=""), pROC::multiclass.roc(labels, as.numeric(preds)))
#         print(paste("roc.",i, sep=""))
#       } else {
#         assign(paste("roc.",i, sep=""), pROC::roc(labels, as.numeric(preds)))
#         print(paste("roc.",i), sep = "")
#       }
#       i = i + 1
#     }
#   }
#
#   i = i -1
#   ################# see plot(a$ROCs[2][1][[1]]) ################
#
#   #Plot ROC curves side by side
#   plotsToReturn <- vector(mode = "list", i-1)
#   if (multi_class) {
#     for (count in 1:i) {
#       # adjust for the first one being outside the loop
#       rs <- get(paste("roc.", count, sep=""))[['rocs']]
#       plotsToReturn[[count]] <- pROC::roc(rs[[1]])
#     }
#   } else {
#     for (count in 1:i) {
#       # Do regular ROC here
#       plotsToReturn[[count]] = get(paste("roc.", count, sep=""))
#     }
#   }
#
#   #legend("bottomright",legend = c("Two-Factor (0.5825)","Full GLM (0.6841)", "SVM1 (0.7553)", "*SVM validate1 (0.7581)", "Neural Net (.8242)"), cex = .7,
#   #      col = c("blue", "red", "green", "purple"), lty = c(1), ncol = 1, text.font = 4, box.lty = 0)
#
#   return(plotsToReturn)
# }
