#' ModelComparisons()
ModelComparison <- function(modelList, multi_class, force_prepped, diff_names=NULL) {
  # we can add our own integrity checks
  comparison <- list()
  if (class(diff_names) == "NULL") {
    model_list = modelList
  } else {
    model_list = modelList
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

#' This function predict on many different machine learning models
#' @param trainingSet the dataset to be trained on
#' @param trainingClasses the labels of the training set
#' @keywords
#' @export
#' @examples
#' predict()
predict.ModelComparison <- function(object, newdata, ...) {
  # # check to see if function is good
  # is_prepped <- sapply(training_data, function(x) (is.numeric(x) || length(levels(x)) <= 2))
  #
  # # Data is not in one hot encoding - try to do it
  # if (sum(is_prepped) != ncol(training_data)) {
  #   training_data = prepData(training_data)
  #   forced_prepared = T
  # } else {
  #   forced_prepared = F
  # }


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
    pred_basic <- list()
    for (model in object$model_list) {
      i = i + 1
      pred_basic[[i]] <- predict(model, newdata, type="prob")
    }
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
  modelVec <- list()
  out <- tryCatch(
    {
      message("Building...")
      if (build.flags["svmlinear"]) {
        svmLinear <- caret::train(training_data, training_classes, method = "svmLinear",
                                  trControl=trctrl,
                                  preProcess = c("center", "scale"),
                                  tuneLength = tune_length)

        modelVec[["svmLinear"]] = svmLinear
      }
      if (build.flags["neuralnet"]) {
        # capture output to reduce the excessive output this package gives in training
        capture.output(neuralNet <- caret::train(training_data, training_classes, method = "nnet",
                                  trControl=trctrl,
                                  preProcess = c("center", "scale"),
                                  tuneLength = tune_length))
        modelVec[["neuralNet"]] = neuralNet
      }

      if (build.flags["glmnet"]) {
        # prepare data for a GLMNET
        train <- data.frame(training_data, training_classes)
        x.m <- model.matrix( ~.+0, training_data)
        glmnet <- caret::train(x.m, training_classes,  method = "glmnet",
                               trControl=trctrl,
                               preProcess = c("center", "scale"),
                               tuneLength = tune_length)
        modelVec[["glmnet"]] = glmnet

      }

      if (build.flags["randomforest"]) {
        randomForest <- caret::train(training_data, training_classes, method = "rf",
                                     trControl=trctrl,
                                     preProcess = c("center", "scale"),
                                     tuneLength = tune_length)
        modelVec[["randomForest"]] = randomForest
      }

      if (!multi_class) {
        if (build.flags["glm"]) {
          glm_model <- caret::train(training_data, as.factor(training_classes), method = "glm",
                                    trControl=trctrl,
                                    preProcess = c("center", "scale"),
                                    tuneLength = tune_length)
          modelVec[["glm"]] = glm_model

        }
      } else {
        if (build.flags["glm"]) {
          glm_model = NULL
          # TODO build a multiclass
        }
      }

      message("Models are done building")
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
    build.glmnet = TRUE
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

GetTrainingInfo <- function(trainingSet, training_classes_input, validation, trctrl.given) {
  # Get the method of validation and prepare testing and training sets
  split <- regmatches(validation, regexpr("[0-9][0-9]/[0-9][0-9]",validation))
  if (!identical(split, character(0))) {
    # take the first two characters and turn them into a percent
    percent = as.numeric(substr(split, start = 1, stop = 2)) / 100
    # partition the data into training and testing data stratified by class
    trainIndex <- caret::createDataPartition(training_classes_input, p=percent, list=F)
    # get the dataframes
    training_data <- trainingSet[trainIndex,]
    testing_data <- trainingSet[-trainIndex,]
    # get the labels
    training_classes <- training_classes_input[trainIndex]
    testing_classes <- training_classes_input[-trainIndex]
    # we will take care of the validation
    trctrl <- caret::trainControl(savePredictions = "final", classProbs =  TRUE)

  } else {
    # we don't need a specific testing set
    training_data = trainingSet
    testing_data <- trainingSet
    training_classes <- training_classes_input
    if (validation == "cv") {
      trctrl <- caret::trainControl(method = "cv", savePredictions = "final", classProbs=TRUE)
    } else {
      trctrl <- caret::trainControl(savePredictions = "final", classProbs=TRUE)
    }
  }
  if (class(trctrl.given) != "character") {
    trctrl = trctrl.given
  }
  return(list(training_data, training_classes, trctrl))
}

#' This function evalutates many different machine learning models and returns those models with comparison charts
#' @param trainingSet the dataset to be trained on
#' @param trainingClasses the labels of the training set
#' @keywords
#' @export
#' @examples
#' getModelComparisons()
getModelComparisons <-function(trainingSet, training_classes_input, validation="80/20", modelList="fast", trctrl="none") {
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

  training_info <- GetTrainingInfo(trainingSet, training_classes_input, validation, trctrl)
  training_data = training_info[[1]]
  training_classes = training_info[[2]]
  trctrl = training_info[[3]]

  build.flags <- GetBuildFlags(modelList)

  message("Settings configured successfully")
  tune_length = 1

  modelVec = buildModels(training_data, training_classes, trctrl,
                       tune_length, multi_class, build.flags, force_prepared = forced_prepared)
  modelComp <- ModelComparison(modelVec, multi_class, forced_prepared)
  return(modelComp)

}


convertToComparison <- function(model_list, multi_class) {
  if (anyNA(model_list) || anyNA(names(model_list))) {
    stop("One of the models or model names is NA.  Please fix that and try again")
  }
  return (ModelComparison(model_list, multi_class, F, names(model_list)))
}
