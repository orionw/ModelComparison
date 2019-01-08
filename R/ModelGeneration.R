#' A helper function used by GetModelComparison and ModelComparison to create the
#' ModelComparison object. It gives the object it's class and assigns it it's members
#'
#' @param model.list a list of models that are already trained.  The name of the model.list should
#' be accurate to the names of the models, unless other names are given in diff.names
#' @param multi.class a boolean value of whether the classification output is more than two outputs
#' @param force.prepped a boolean value describing whether the dataset was forced prepared into
#' one hot encoding for training.  Only applicable with categorical data
#' @param diff.names a vector or list containing the names of the model.list models, in that
#' respective order.  This is optional if the names are already attatched to the model.list
#'  and will default to NULL.
#'
#' @return the fully created ModelComparison object
#'
#' @examples
#' # This function is used by GetModelComparison and ModelComparison to create the
#' ModelComparison object. The follow code is what is used by ModelComparison:
#' CreateModelComparison(model.list, multi.class, F, names(model.list))
#'
#' @keywords internal
#' @export
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

#' Gives a summary of the models used in the ModelComparison object
#'
#' @param object the ModelComparison object to get the accuracies from
#'
#' @return a list of numeric values that are the accuracies of the models
#'
#' @examples
#' # see the CreateModelComparison function for it's use as follows:
#'   comparison$accuracy.list <- GetAccuracy(comparison)
#'
#' @keywords internal
#' @export
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

#' Gives a summary of the models used in the ModelComparison object
#'
#' @param extra a boolean value on whether accuracy values will be returned, if present.
#' By default this value is TRUE
#'
#' @return NULL, only prints out
#'
#' @examples
#' # load the csv file for the dataset "titanic"
#' titanic <- PrepareNumericTitanic()
#' # create the ModelComparison object by passing in the training set and training labels
#' comp <- GetModelComparisons(titanic[, -1], titanic[, 1])
#' # predict by passing in the new df for the object to predict on
#' summary(comp)
#'
#' @export
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

#' Predicts values for a ModelComparison object.
#'
#' @param object The ModelComparison object whose models will be predicted on.
#' @param newdata A dataframe of new data that the models will use to predict.
#'
#' @return A list of dataframes, where each dataframe is the predictions from the respective
#'            model for the two output classes.
#'
#' @examples
#' # load the csv file for the dataset "titanic"
#' titanic <- PrepareNumericTitanic()
#' # create the ModelComparison object by passing in the training set and training labels
#' comp <- GetModelComparisons(titanic[, -1], titanic[, 1])
#' # predict by passing in the new df for the object to predict on
#' pred.list <- predict(comp, titanic[, -1])
#'
#' @export
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
    .getClass <- function(model) { return(class(model)[[1]])    }
    names(pred.basic) <- sapply(object$model.list, .getClass)
    return(pred.basic)
  }
}

#' A helper function used by the predict function. Since the many types of models are not
#' uniform in how they are called, this function determines what values to pass into "type"
#' and then returns the predictions.
#'
#' @param model The model to be used for prediction
#' @param newdata A dataframe for the model to predict on
#'
#' @return The predictions for the model on the given data, returned in dataframe format
#'
#' @examples
#' # This function is used by predict.ModelComparison. It's usage is as follows:
#' pred.basic[[i]] = GetPredType(model, newdata)
#'
#' @keywords internal
#' @export
GetPredType <- function(model, newdata) {
  if (class(model)[1] == "naiveBayes" || class(model)[1] == "nnet.formula" ||
      class(model)[1] == "nnet") {
    return(predict(model, newdata, type="raw", prob=TRUE))

  } else if (class(model)[[1]] == "lognet" || class(model)[[1]] == "glmnet") {
    return (predict(model, data.matrix(newdata), type="response", prob=TRUE))

    # SVM's require more work to get the probabilites
  } else if (class(model)[[1]] == "svm.formula" || class(model)[[1]] == "svm") {
    svm.pred <- predict(model, data.matrix(newdata), type="response", decision.values=TRUE)
    #return (svm.pred[1:nrow(newdata)])
    return(attr(svm.pred, "decision.values"))

  } else {
    return (predict(model, newdata, type="prob", prob=TRUE))
  }
}

#' A helper function used by the GetModelComparison function. If the data given is categorical
#' and not in one-hot encoding the model training will fail.  This will attempt to conver the
#' dataset into that format, or, to send an error if it fails to do so.
#'
#' @param training.set the training data that is in the wrong format. This is the same
#' dataframe as in GetModelComparison parameters.
#'
#' @return The dataframe in one-hot encoding.
#'
#' @examples
#' # This function is used by GetModelComparison. It's usage is as follows:
#' training.set = prepData(training.set)
#'
#' @keywords internal
#' @export
prepData <- function(training.set) {
    out <- tryCatch(
      {
        dmy <- caret::dummyVars(" ~ .", data = training.set)
        training.set <- data.frame(predict(dmy, newdata = training.set))
        return(training.set)
      },
      error=function(cond) {
        message("Data set is not numeric or in one hot encoding.  Will try to convert")
        message(cond)
        # Choose a return value in case of error
        return(NA)
      }
    )
  return(out)
}

#' A helper function used by the GetModelComparison function. It will take the given inputs
#' and build the models for the ModelComparison object.
#'
#' @param training.data The training data to build the models.
#' @param training.classes The labels for the training data.
#' @param trctrl The trctrl used for the caret package to train the models.
#' @param tune.length The amound of time to tune to the model. Defaults to 1.
#' @param multi.class A boolean value of whether the model has multiple classification outputs
#' @param build.flags A vector of flags that decide what models to build.
#' @param force.prepared A boolean value describing whether the dataset was forced prepared into
#' one hot encoding for training.  Only applicable with categorical data
#'
#' @return A vector of the models created.
#'
#' @examples
#' # This function is used by GetModelComparison. It's usage is as follows:
#' modelVec = BuildModels(training.data, training.classes, trctrl,
#' tune.length, multi.class, build.flags, force.prepared = forced.prepared)
#'
#' @keywords internal
#' @export
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
        capture.output(neuralNet <- caret::train(training.data, training.classes,
                                                 method = "nnet",
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

      if (build.flags["svmradial"]) {
        svmRadial <- caret::train(training.data, training.classes, method = "svmRadial",
                                     trControl=trctrl,
                                     preProcess = c("center", "scale"),
                                     tuneLength = tune.length)
        modelVec[["svmRadial"]] = svmRadial
      }

      if (build.flags["knn"]) {
        knn <- caret::train(training.data, training.classes, method = "knn",
                                     trControl=trctrl,
                                     preProcess = c("center", "scale"),
                                     tuneLength = tune.length)
        modelVec[["knn"]] = knn
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

#' A helper function used by the GetModelComparison function. It takes the inputs and parses
#' it to see what models to build
#'
#' @param model.list The input parameters from GetModelComparison on what models to create.
#'
#' @return A vector of flags that contain what models to build.  To be used in BuildModels
#' function
#'
#' @examples
#' # This function is used by GetModelComparison. It's usage is as follows:
#' build.flags <- GetBuildFlags(model.list)
#'
#' @keywords internal
#' @export
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

#' A helper function used by the GetModelComparison function.
#'
#' It takes the given inputs, data, and trctrl needed to build the models.
#'
#' @param training.set The training data to build the models.
#' @param training.classes.input The labels for the training data.
#' @param validation The type of model validation: cross-validation ("cv"), or a training
#' split (in the form XX/XX where XX is a two digit percent).
#' @param trctrl.given The trctrl given in GetModelComparison parameters for custom training.
#'
#' @return A vector containg the following for training the models. It is given in this order:
#' training.data, training.classes, trctrl
#'
#' @examples
#' # This function is used by GetModelComparison. It's usage is as follows:
#' training.info <- GetTrainingInfo(training.set, training.classes.input, validation, trctrl)
#'
#' @keywords internal
#' @export
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

#' A function to build and return a ModelComparison object.
#'
#' This function does not take any
#' pre-built models, instead it creates them based on input to model.list.
#'
#' @param training.set The training data to build the models.
#' @param training.classes.input The labels for the training data.
#' @param validation The type of model validation: cross-validation ("cv"), or a training
#' split (in the form XX/XX where XX is a two digit percent).
#' @param model.list A vector or list of characters that describe what models to build.
#' Models include "neuralnet", "svmlinear", "svmradial", "knn", "randomforest", 'glmnet",
#' and 'glm." Keywords include "fast" (glmnet, svmlinear), "all", and "expensive"
#' (neuralnet, svmradial, glmnet, and randomforest)
#' @param trctrl The trctrl used for the caret package to train the models. Defaults to a basic
#' version. Used for customized training options including parallelization, etc.
#'
#' @return A fully created ModelComparison object.
#'
#' @examples
#' titanic <- PrepareNumericTitanic()
#'
#' # create the models
#' comp <- GetModelComparisons(titanic[, -1], titanic[, 1], validation="80/20",
#'                             model.list="fast")
#'
#' @export
GetModelComparisons <-function(training.set, training.classes.input, validation="80/20",
                               model.list="fast", trctrl="none") {
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

#' A function to return a ModelComparison object.
#'
#'   This function take pre-built models, and puts
#' them into a ModelComparison object.
#'
#' @param model.list The training data to build the models.
#' @param multi.class A boolean of whether the models predict a multi-outcome response
#'
#' @return A ModelComparison object.
#'
#' @examples
#' models <- list(model1, model2, model3)
#'
#' names(models) <- c("NeuralNet", "K-NN", "SVM")
#'
#' comp <- ModelComparison(models, F)
#'
#' @export
ModelComparison <- function(model.list, multi.class) {
  if (anyNA(model.list) || anyNA(names(model.list))) {
    stop("One of the models or model names is NA.  Please fix that and try again")
  }
  return (CreateModelComparison(model.list, multi.class, F, names(model.list)))
}
