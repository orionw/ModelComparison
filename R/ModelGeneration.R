#' ModelComparisons()
CreateModelComparison <- function(model.list, multi.class, force.prepped, diff.names=NULL) {
  # we can add our own integrity checks
  comparison <- list()
  if (class(diff.names) == "NULL") {
    model.list = model.list
  } else {
    model.list = model.list
    names(model.list) <- diff.names
  }
  # give object its members
  comparison$model.list <- list()
  comparison$model.list <- as.list(model.list)
  comparison$force.prepared = force.prepped
  comparison$.multi.class <- multi.class
  class(comparison) <- "ModelComparison"
  comparison$accuracy.list <- GetAccuracy(comparison)
  return(comparison)
}

GetAccuracy <- function(object) {
  out <- tryCatch(
    {
      accuracy.list <- list()
      i = 0
      for (model in object$model.list) {
        if (class(model)[[1]] == "Ensemble") {
          # TODO implement this
          return(NULL)
        }
        i = i + 1
        accuracy.list[[i]] <- model$results["Accuracy"]
      }
      names(accuracy.list) <- names(object$model.list)
      return(accuracy.list)
    },
    error=function(cond) {
      # model's accuracy was not given
      return(NULL)
    }
  )
  return(out)

}

summary.ModelComparison <- function(object, extra=TRUE, ...) {
  start <- "This object is a comparison of the following models:"
  cat(start)
  cat("\n")
  models <- paste(names(object$model.list), collapse = ", ")
  cat(models)
  if (any(!is.na(object$accuracy.list)) && extra) {
    cat("\n\n")
    cat("with respective accuracies of:")
    cat("\n")
    for (acc in object$accuracy.list) {
      cat(as.character(acc[[1]]))
      cat(" ")
    }
  }
  if (any(!is.na(object$auc.list))) {
    cat("\n\n")
    cat("and with respective AUC's of: ")
    cat("\n")
    for (auc in object$auc.list) {
      cat(as.character(auc[[1]]))
      cat(" ")
    }
  }
}

#' This function predict on many different machine learning models
#' @param training.set the dataset to be trained on
#' @param trainingClasses the labels of the training set
#' @keywords
#' @export
#' @examples
#' predict()
predict.ModelComparison <- function(object, newdata, ...) {
  # # check to see if function is good
  # is.prepped <- sapply(training.data, function(x) (is.numeric(x) || length(levels(x)) <= 2))
  #
  # # Data is not in one hot encoding - try to do it
  # if (sum(is.prepped) != ncol(training.data)) {
  #   training.data = prepData(training.data)
  #   forced.prepared = T
  # } else {
  #   forced.prepared = F
  # }


  predictions.list = list()
  i = 0
  if (object$.multi.class) {
    # do something TODO
    message("predicting on a multi-class outcome")
    for (model in object$model.list) {
      i = i + 1
    }
  } else {
    # predict over the list of models
    pred.basic <- list()
    for (model in object$model.list) {
      i = i + 1
      pred.basic[[i]] = GetPredType(model, newdata)
    }
    return(pred.basic)
  }
}

GetPredType <- function(model, newdata) {
  if (class(model)[1] == "naiveBayes" || class(model)[1] == "nnet.formula" ||
      class(model)[1] == "nnet") {
    return(predict(model, newdata, type="raw", prob=TRUE))

  } else if (class(model)[[1]] == "lognet" || class(model)[[1]] == "glmnet") {
    return (predict(model, data.matrix(newdata), type="response", prob=TRUE))

  } else {
    return (predict(model, newdata, type="prob", prob=TRUE))
  }
}

prepData <- function(training.set) {
    out <- tryCatch(
      {
        dmy <- caret::dummyVars(" ~ .", data = training.set)
        training.set <- data.frame(predict(dmy, newdata = training.set))
        return(training.set)
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


BuildModels <- function(training.data, training.classes, trctrl,
                     tune.length, multi.class, build.flags, force.prepared = F ) {
  modelVec <- list()
  out <- tryCatch(
    {
      message("Building...")
      if (build.flags["svmlinear"]) {
        svmLinear <- caret::train(training.data, training.classes, method = "svmLinear",
                                  trControl=trctrl,
                                  preProcess = c("center", "scale"),
                                  tuneLength = tune.length)

        modelVec[["svmLinear"]] = svmLinear
      }
      if (build.flags["neuralnet"]) {
        # capture output to reduce the excessive output this package gives in training
        capture.output(neuralNet <- caret::train(training.data, training.classes, method = "nnet",
                                  trControl=trctrl,
                                  preProcess = c("center", "scale"),
                                  tuneLength = tune.length))
        modelVec[["neuralNet"]] = neuralNet
      }

      if (build.flags["glmnet"]) {
        # prepare data for a GLMNET
        train <- data.frame(training.data, training.classes)
        x.m <- model.matrix( ~.+0, training.data)
        glmnet <- caret::train(x.m, training.classes,  method = "glmnet",
                               trControl=trctrl,
                               preProcess = c("center", "scale"),
                               tuneLength = tune.length)
        modelVec[["glmnet"]] = glmnet

      }

      if (build.flags["randomforest"]) {
        randomForest <- caret::train(training.data, training.classes, method = "rf",
                                     trControl=trctrl,
                                     preProcess = c("center", "scale"),
                                     tuneLength = tune.length)
        modelVec[["randomForest"]] = randomForest
      }

      if (!multi.class) {
        if (build.flags["glm"]) {
          glm <- caret::train(training.data, as.factor(training.classes), method = "glm",
                                    trControl=trctrl,
                                    preProcess = c("center", "scale"),
                                    tuneLength = tune.length)
          modelVec[["glm"]] = glm

        }
      } else {
        if (build.flags["glm"]) {
          glm = NULL
          # TODO build a multiclass
        }
      }

      message("Models are done building")
      return(modelVec)

    },
    error=function(cond) {
      if (force.prepared) {
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


GetBuildFlags <- function(model.list) {
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
  model.list <- sapply(model.list, tolower)
  if (sum(is.element(model.list, "fast"))) {
    build.glmnet = TRUE
    build.svmlinear = TRUE
  } else if (sum(is.element(model.list, "all"))) {
    build.glm = TRUE
    build.svmlinear = TRUE
    build.svmradial = TRUE
    build.neuralnet = TRUE
    build.knn = TRUE
    build.glmnet = TRUE
    build.randomforest = TRUE
  } else if (sum(is.element(model.list, "expensive"))) {
    build.svmradial = TRUE
    build.neuralnet = TRUE
    build.glmnet = TRUE
    build.randomforest = TRUE
  }
  if (sum(is.element(model.list, "neuralnet"))) {
    build.neuralnet = TRUE
  }
  if (sum(is.element(model.list, "svmlinear"))) {
    build.svmlinear = TRUE
  }
  if (sum(is.element(model.list, "svmradial"))) {
    build.svmradial = TRUE
  }
  if (sum(is.element(model.list, "knn"))) {
    build.knn = TRUE
  }
  if (sum(is.element(model.list, "randomforest"))) {
    build.randomforest = TRUE
  }
  if (sum(is.element(model.list, "glmnet"))) {
    build.glmnet = TRUE
  }
  if (sum(is.element(model.list, "glm"))) {
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

GetTrainingInfo <- function(training.set, training.classes.input, validation, trctrl.given) {
  # Get the method of validation and prepare testing and training sets
  split <- regmatches(validation, regexpr("[0-9][0-9]/[0-9][0-9]",validation))
  if (!identical(split, character(0))) {
    # take the first two characters and turn them into a percent
    percent = as.numeric(substr(split, start = 1, stop = 2)) / 100
    # partition the data into training and testing data stratified by class
    trainIndex <- caret::createDataPartition(training.classes.input, p=percent, list=F)
    # get the dataframes
    training.data <- training.set[trainIndex,]
    testing.data <- training.set[-trainIndex,]
    # get the labels
    training.classes <- training.classes.input[trainIndex]
    testing.classes <- training.classes.input[-trainIndex]
    # we will take care of the validation
    trctrl <- caret::trainControl(savePredictions = "final", classProbs =  TRUE)

  } else {
    # we don't need a specific testing set
    training.data = training.set
    testing.data <- training.set
    training.classes <- training.classes.input
    if (validation == "cv") {
      trctrl <- caret::trainControl(method = "cv", savePredictions = "final", classProbs=TRUE)
    } else {
      trctrl <- caret::trainControl(savePredictions = "final", classProbs=TRUE)
    }
  }
  if (class(trctrl.given) != "character") {
    trctrl = trctrl.given
  }
  return(list(training.data, training.classes, trctrl))
}

#' This function evalutates many different machine learning models and returns those models with comparison charts
#' @param training.set the dataset to be trained on
#' @param trainingClasses the labels of the training set
#' @keywords
#' @export
#' @examples
#' GetModelComparisons()
GetModelComparisons <-function(training.set, training.classes.input, validation="80/20", model.list="fast", trctrl="none") {
  # check to see if function is good
  is.prepped <- sapply(training.set, function(x) (is.numeric(x) || length(levels(x)) <= 2))

  # Data is not in one hot encoding - try to do it
  if (sum(is.prepped) != ncol(training.set)) {
    training.set = prepData(training.set)
    forced.prepared = T
  } else {
    forced.prepared = F
  }

  # prep the data if not already a factor
  training.classes.input = as.factor(training.classes.input)
  set.seed(sample(1:9999999, 1))
  multi.class = (nlevels(training.classes.input) > 2)

  training.info <- GetTrainingInfo(training.set, training.classes.input, validation, trctrl)
  training.data = training.info[[1]]
  training.classes = training.info[[2]]
  trctrl = training.info[[3]]

  build.flags <- GetBuildFlags(model.list)

  message("Settings configured successfully")
  tune.length = 1

  modelVec = BuildModels(training.data, training.classes, trctrl,
                       tune.length, multi.class, build.flags, force.prepared = forced.prepared)
  modelComp <- CreateModelComparison(modelVec, multi.class, forced.prepared)
  return(modelComp)

}

ModelComparison <- function(model.list, multi.class) {
  if (anyNA(model.list) || anyNA(names(model.list))) {
    stop("One of the models or model names is NA.  Please fix that and try again")
  }
  return (CreateModelComparison(model.list, multi.class, F, names(model.list)))
}
