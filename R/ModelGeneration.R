#' ModelComparisons()
ModelComparison <- function(ModelList, multi_class, force_prepped, diff_names=NULL) {
  # we can add our own integrity checks
  comparison <- list()
  if (class(diff_names) == "NULL") {
    model_list <- list(svmLinear = ModelList[["svmLinear"]],
                  neuralNet = ModelList[["neuralNet"]],
                  glmnet = ModelList[["glmnet"]],
                  randomForest = ModelList[["randomForest"]],
                  glm = ModelList[["glm"]])
  } else {
    model_list = ModelList
    names(model_list) <- diff_names
  }
  # give object its members
  comparison$model_list <- list()
  comparison$model_list <- as.list(model_list)
  comparison$force_prepared = force_prepped
  comparison$.multi_class <- multi_class
  class(comparison) <- "ModelComparison"
  return(comparison)
}

#' This function evalutates many different machine learning models and returns those models with comparison charts
#' @param trainingSet the dataset to be trained on
#' @param trainingClasses the labels of the training set
#' @keywords
#' @export
#' @examples
#' plot()
plot.ModelComparison <- function(object, training_data, labels, predictions="empty") {
  # check to see if the training_data hasn't been prepped and if it was trained on prepped data
  is_prepped <- sapply(training_data, function(x) (is.numeric(x) || length(levels(x)) <= 2))

  if (object$force_prepared || sum(is_prepped) != ncol(training_data)) {
    # Data is not in one hot encoding - try to do it
    training_data = prepData(training_data)
  }

  if (object$.multi_class == TRUE) {
    # do stuff later
    message("dataset is multi-class")
  } else {
      if (predictions == "empty") {
        # predictions somehow failed to happen - predict in here
        pred_basic <- predict(object$model_list, newdata=training_data, type="prob")
      } else {
        # use the given predictions
        pred_basic = predictions
      }
      i = 0
      colorPal = rainbow(length(object$model_list))
      for (model in object$model_list) {
        i = i + 1
        if (!is.null(model)) {
          # if given in dataframe format, reduce to vector
          if (class(pred_basic[[i]]) == "data.frame") {
            pred_basic[[i]] <- pred_basic[[i]][, 1]
          }
          if (i == 1) {
              # do this to init the plot - for the first model
              assertthat::are_equal(length(labels), length(pred_basic[[i]]))
              roc_plot <- pROC::roc(labels, pred_basic[[i]])
              plot(roc_plot, col = colorPal[i], title="ROC Comparison")
          } else {
              assertthat::are_equal(length(labels), length(pred_basic[[i]]))
              roc_plot <- pROC::roc(labels, pred_basic[[i]])
              plot(roc_plot, add = T, col = colorPal[i])
          }
        }
      legend("topright", title="Model Type", legend=names(object$model_list),
               col=colorPal, lty=1:2, cex=0.8)
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
predict.ModelComparison <- function(object, training_data) {
  predictions_list = list()
  i = 0
  if (object$.multi_class) {
    # do something TODO
    message("predicting on a multi-class outcome")
    for (model in object$model_list) {
      i = i + 1
    }
  } else {
    # predict over the list of models
    pred_basic <- predict(object$model_list, newdata=training_data, type="prob")
    return(pred_basic)
  }
}

prepData <- function(training_set) {
    out <- tryCatch(
      {
        dmy <- caret::dummyVars(" ~ .", data = training_set)
        training_set <- data.frame(predict(dmy, newdata = training_set))
        return(training_set)
      },
      error=function(cond) {
        message(paste("Data set is not numeric or in one hot encoding.  Will try to convert", url))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(NA)
      }
    )
  return(out)
}


buildModels <- function(training_data, training_classes, trctrl,
                     tune_length, multi_class, build.flags, force_prepared = F ) {
  out <- tryCatch(
    {
      svmLinear <- caret::train(training_data, training_classes, method = "svmLinear",
                                trControl=trctrl,
                                preProcess = c("center", "scale"),
                                tuneLength = tune_length)

      # capture output to reduce the excessive output this package gives in training
      capture.output(neuralNet <- caret::train(training_data, training_classes, method = "nnet",
                                trControl=trctrl,
                                preProcess = c("center", "scale"),
                                tuneLength = tune_length))

      # prepare data for a GLMNET
      train <- data.frame(training_data, training_classes)
      x.m <- model.matrix( ~.+0, training_data)
      glmnet <- caret::train(x.m, training_classes,  method = "glmnet",
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

      message("Models are done building")

      # get the visuals for all the models
      modelVec = list(svmLinear, neuralNet, glmnet, randomForest, glm_model)
      return(modelVec)

    },
    error=function(cond) {
      if (force_prepared) {
        message("Forced conversion to one hot encoding did not work - convert and try again")
        message(cond)
      } else {
        message("Error in building models: ")
        message(cond)
      }
      # Choose a return value in case of error
      return(NA)
    }
  )
  return(out)
}


GetBuildFlags <- function(modelList) {
  # Flags include:
  # neuralnet, svmlinear, svmradial, knn, randomforest, glmnet, glm
  # keywords include "fast", "all", "expensive"

  build.glm = FALSE
  build.glmnet = FALSE
  build.randomforest = FALSE
  build.knn = FALSE
  build.svmradial = FALSE
  build.svmlinear = FALSE
  build.neuralnet = FALSE
  # turn all to lowercase
  modelList <- sapply(modelList, tolower)
  if (sum(is.element(modelList, "fast"))) {
    build.glm = TRUE
    build.svmlinear = TRUE
  } else if (sum(is.element(modelList, "all"))) {
    build.glm = TRUE
    build.svmlinear = TRUE
    build.svmradial = TRUE
    build.neuralnet = TRUE
    build.knn = TRUE
    build.glmnet = TRUE
    build.randomforest = TRUE
  } else if (sum(is.element(modelList, "expensive"))) {
    build.svmradial = TRUE
    build.neuralnet = TRUE
    build.glmnet = TRUE
    build.randomforest = TRUE
  }
  if (sum(is.element(modelList, "neuralnet"))) {
    build.neuralnet = TRUE
  }
  if (sum(is.element(modelList, "svmlinear"))) {
    build.svmlinear = TRUE
  }
  if (sum(is.element(modelList, "svmradial"))) {
    build.svmradial = TRUE
  }
  if (sum(is.element(modelList, "knn"))) {
    build.knn = TRUE
  }
  if (sum(is.element(modelList, "randomforest"))) {
    build.randomforest = TRUE
  }
  if (sum(is.element(modelList, "glmnet"))) {
    build.glmnet = TRUE
  }
  if (sum(is.element(modelList, "glm"))) {
    build.glm = TRUE
  }
  # assign flags to vector
    flag.vector <- vector()
  flag.vector["glm"] = build.glm
  flag.vector["glmnet"] = build.glmnet
  flag.vector["randomforest"] = build.randomforest
  flag.vector["knn"] = build.knn
  flag.vector["svmradial"] = build.svmradial
  flag.vector["svmlinear"] = build.svmlinear
  flag.vector["neuralnet"] = build.neuralnet
  return(flag.vector)
}


#' This function evalutates many different machine learning models and returns those models with comparison charts
#' @param trainingSet the dataset to be trained on
#' @param trainingClasses the labels of the training set
#' @keywords
#' @export
#' @examples
#' getModelComparisons()
getModelComparisons <-function(trainingSet, training_classes_input, validation="80/20", modelList="fast", dataSetSize="small") {
  # check to see if function is good
  is_prepped <- sapply(trainingSet, function(x) (is.numeric(x) || length(levels(x)) <= 2))

  # Data is not in one hot encoding - try to do it
  if (sum(is_prepped) != ncol(trainingSet)) {
    trainingSet = prepData(trainingSet)
    forced_prepared = T
  } else {
    forced_prepared = F
  }

  # prep the data if not already a factor
  training_classes_input = as.factor(training_classes_input)
  set.seed(sample(1:9999999, 1))
  multi_class = (nlevels(training_classes_input) > 2)

  build.flags <- GetBuildFlags(modelList)

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
    trctrl <- caret::trainControl(method = "none", savePredictions = T, classProbs =  TRUE)
  } else {
    # we don't need a specific testing set
    training_data = trainingSet
    testing_data <- trainingSet
    testing_classes <- training_classes
    if (validation == "cv") {
      trctrl <- caret::trainControl(method = "cv", savePredictions = T, classProbs=TRUE)
    } else {
      trctrl <- caret::trainControl(method = "none", savePredictions = T, classProbs=TRUE)
    }
  }

  message("Settings configured successfully")
  tune_length = 1

  modelVec = buildModels(training_data, training_classes, trctrl,
                       tune_length, multi_class, build.flags, force_prepared = forced_prepared)
  names(modelVec) <- c("svmLinear", "neuralNet", "glmnet", "randomForest", "glm")
  modelComp <- ModelComparison(modelVec, multi_class, forced_prepared)
  return(modelComp)

}


convertToComparison <- function(model_list, multi_class) {
  if (anyNA(model_list) || anyNA(names(model_list))) {
    stop("One of the models or model names is NA.  Please fix that and try again")
  }
  return (ModelComparison(model_list, multi_class, F, names(model_list)))
}
