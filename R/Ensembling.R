#' A function for extracting the prediction values from the models.
#'
#' Each model returns the prediction as a DataFrame and we need it as a list or vector.
#'
#' @param pred A list containing a DataFrame of predictions.
#'
#' @return A vector containing the predictions.
#'
#' @examples
#' # This function is used by predict.Ensemble. It's usage is as follows:
#' # preds[[i]] <-  StripPredictions(predict(model, newdata = newdata, type="prob"))
#'
#' @keywords internal
#' @export
StripPredictions <- function(pred) {
  if (class(pred)[[1]] == "list") {
    realPredictions <- list(length = length(pred))
    i = 0
    for (model.pred in pred) {
      i = i + 1
      realPredictions[[i]] = model.pred[, 1]
    }
    return(realPredictions)
  } else {
    return(pred[, 1])
  }
}

#' Gives a summary of the models used in the Ensemble object
#'
#' @param object The Ensemble object to be summarized.
#' @param ... Other arguments, if needed

#'
#' @return NULL, only prints out
#'
#' @examples
#' # prepare a binary classification dataset
#' iris <- PrepareIris()
#' # create the models
#' comp <- GetModelComparisons(iris[,1:4], iris[,5])
#' # use the models in the comparison to form a one model Ensemble
#' ensem <- Ensemble(comp$model.list, "majorityWeight", iris[,1:4], iris[,5])
#' summary(ensem)
#'
#' @export
summary.Ensemble <- function(object, ...) {
  start <- "This object is a Ensemble of the following models: "
  cat(start)
  cat("\n")
  models <- paste(names(object$models), collapse = ", ")
  cat(models)
  cat("\n\n")
  cat("And uses a ")
  cat(object$.voting.type)
  cat(" voting type")
}

#' Predicts values for a Ensemble object.
#'
#' This involves calling the voting function specified
#' at Ensemble creation or can be overriden by parameters.
#'
#' @param object The Ensemble object whose models will be predicted on.
#' @param newdata A dataframe of new data that the models will use to predict.
#' @param voting.type The way that the Ensemble will vote - default is what was given
#' at creation of the object but it can be overriden by passing in "averageVote", "majorityVote"
#' or "majorityWeight."
#' @param ... Other arguments, if needed
#'
#' @return a list of predictions, voted on by all the models into one final prediction.
#'
#' @examples
#' # load the csv file for the dataset "titanic"
#' # titanic <- PrepareNumericTitanic()
#' # create the ModelComparison object by passing in the training set and training labels
#' # comp <- GetModelComparisons(titanic[, -1], titanic[, 1])
#' # use the models in the comparison to form a one model Ensemble
#' # ensem <- Ensemble(comp$model.list, "majorityWeight", iris[,1:4], iris[,5])
#' # predict by passing in the new df for the object to predict on
#' # pred.list <- predict(comp, titanic[, -1], voting.type="averageVote")
#'
#' @export
predict.Ensemble <- function(object, newdata, voting.type="default", ...) {
  preds <- list(length=length(object$models))
  # predict on list of models
  i = 0
  for (model in object$models) {
    i = i + 1
    preds[[i]] <-  StripPredictions(GetPredType(model, newdata))
  }

    # input overrides set voting type
  if (voting.type != "default") {
      if (voting.type == "majorityVote") {
        return(MajorityVote(preds))
    } else if (voting.type == "majorityWeight") {
      return(MajorityWeight(preds, object$weight.list))
    } else if (voting.type == "averageVote") {
      return((AverageVote(preds)))
    } else {
      stop("Voting type was not correctly specified.  Please use 'majorityVote',
           'majorityWeight', or 'averageVote'")
    }
  } else if (object$.voting.type == "majorityVote") {
    # round the values before passing them in
    return(MajorityVote(preds))

  } else if (object$.voting.type == "majorityWeight") {
    # not majority vote, use weighting
    return(MajorityWeight(preds, object$weight.list))

  } else if (object$.voting.type == "averageVote"){
      # use average weighting
    return((AverageVote(preds)))
  }
}

#' A function for voting.  This is used by the Ensemble to predict on new data if the voting
#' type is the same as this function name.
#'
#' @param list.of.predictions A list of lists where each sublist is a prediction from one of
#' the models in the Ensemble.
#'
#' @return A vector containing the predictions from the combined vote of all models
#'
#' @examples
#' # This function is used by predict.Ensemble. It's usage is as follows:
#' # MajorityVote(preds)
#'
#' @keywords internal
#' @export
MajorityVote <- function(list.of.predictions){
  if (anyNA(list.of.predictions, recursive = TRUE)) {
    stop("There are NA's in this prediction.  Please predict correct classes")
  }
  # if predictions are given in dataframe form, strip them to vector form
  if (class(list.of.predictions[[1]]) == "data.frame") {
    list.of.predictions <- StripPredictions(list.of.predictions)
  }
  votesFinal <- list(length=length(list.of.predictions[[1]]))
  for(i in 1:length(list.of.predictions[[1]])){
    # for length of the predictions
    vote = vector(mode="numeric", length=length(list.of.predictions))
    j = 0
    for (model.pred in list.of.predictions) {
      j = j + 1
      # for each model at that prediction index
      vote[[j]] = round(model.pred[[i]])
    }
    votesFinal[[i]] = (names(which.max(table(vote))))
  }
  return((as.numeric(votesFinal)))
}

#' A function for voting.  This is used by the Ensemble to predict on new data if the voting
#' type is the same as this function name.
#'
#' @param list.of.predictions A list of lists where each sublist is a prediction from one of
#' the models in the Ensemble.
#' @param weight.list A list of weights for the respective models, to be used in the voting.
#'
#' @return A vector containing the predictions from the combined weighted vote of all models
#'
#' @examples
#' # This function is used by predict.Ensemble. It's usage is as follows:
#' # MajorityWeight(preds, Ensemble$weights)
#'
#' @keywords internal
#' @export
MajorityWeight <- function(list.of.predictions, weight.list){
  if (anyNA(list.of.predictions, recursive = TRUE)) {
    stop("There are NA's in this prediction.  Please predict correct classes")
  }
  # if predictions are given in dataframe form, strip them to vector form
  if (class(list.of.predictions[[1]]) == "data.frame") {
    list.of.predictions <- StripPredictions(list.of.predictions)
  }
  votesFinal <- list(length=length(list.of.predictions[[1]]))
  for(i in 1:length(list.of.predictions[[1]])){
    # for length of the predictions
    vote = vector(mode="numeric", length=length(list.of.predictions))
    j = 0
    for (model.pred in list.of.predictions) {
      j = j + 1
      # for each model at that prediction index
      vote[[j]] = model.pred[[i]] * weight.list[[j]]
    }
    sum.preds = sum(vote)
    if (sum.preds > 1) {
      sum.preds = 1
    } else if (sum.preds < 0) {
      sum.preds = 0
    }
    votesFinal[[i]] = sum.preds
  }
  # remove all names associated
  names(votesFinal) <- c()
  return(as.numeric(votesFinal))
}

#' A function for voting.  This is used by the Ensemble to predict on new data if the voting
#' type is the same as this function name.
#'
#' @param list.of.predictions A list of lists where each sublist is a prediction from one of
#' the models in the Ensemble.
#'
#' @return A vector containing the predictions from the combined vote of all models
#'
#' @examples
#' # This function is used by predict.Ensemble. It's usage is as follows:
#' # AverageVote(preds)
#'
#' @keywords internal
#' @export
AverageVote <- function(list.of.predictions){
  if (anyNA(list.of.predictions, recursive = TRUE)) {
    stop("There are NA's in this prediction.  Please predict correct classes")
  }
  # if predictions are given in dataframe form, strip them to vector form
  if (class(list.of.predictions[[1]]) == "data.frame") {
    list.of.predictions <- StripPredictions(list.of.predictions)
  }
  votesFinal <- list(length=length(list.of.predictions[[1]]))
  for(i in 1:length(list.of.predictions[[1]])){
    # for length of the predictions
    vote = vector(mode="numeric", length=length(list.of.predictions))
    j = 0
    for (model.pred in list.of.predictions) {
      j = j + 1
      # for each model at that prediction index
      vote[[j]] = model.pred[[i]]
    }
    sum.preds = sum(vote) / length(list.of.predictions)
    if (sum.preds > 1) {
      sum.preds = 1
    } else if (sum.preds < 0) {
      sum.preds = 0
    }
    votesFinal[[i]] = sum.preds
  }
  # remove all names associated
  names(votesFinal) <- c()
  return(as.numeric(votesFinal))
}


#' A quick helper function to flip the factors of 0's and 1's
#'
#' @param pred The predictions from the model
#' @return The predictions vector but in the correct format
#'
#' @examples
#' # This function is used by GetWeightsFromTestSet It's usage is as follows:
#' # test.set <- GetFactorEqual(test.set)
#'
#' @keywords internal
#' @export
GetFactorEqual <- function(pred) {
  if (class(levels(pred)[[1]]) == "character") {
    levels(pred) <- 1:length(levels(pred))
    pred <- as.numeric(pred)
    # scale to 1 and 0
    pred <- pred - 1
    # flip the signs to make correct
    pred <- 1 - pred
    return(as.factor(pred))
  } else {
    # already numeric, return
    return (as.factor(pred))
  }
}

#' A helper function used by the GetModelWeights function. It uses the given data to predict
#' and pull the metric from the data.
#'
#' @param ensemble The training data to build the models.
#' @param df.train The predictions of the model, given in DataFrame format.
#' @param test.set The data test set so that metrics can be calculated.
#' @param train.type The metric to be used in getting model weights. We pull this metric
#' from the confusion matrix.
#'
#' @return A vector containg weights for the Ensemble's models.
#'
#' @examples
#' # This function is used by Ensemble. It's usage is as follows:
#' # return(GetWeightsFromTestingSet(ensemble, weights, test.set, train.type))
#'
#' @keywords internal
#' @export
GetWeightsFromTestingSet <- function(ensemble, df.train, test.set, train.type) {
  i = 0
  preds = list(length = length(ensemble$models))
  fakeModelComp <- list()
  class(fakeModelComp) <- "ModelComparison"
  fakeModelComp$model.list <- ensemble$models
  fakeModelComp$.multi.class <- FALSE
  preds <- predict.ModelComparison(fakeModelComp, df.train, type="prob")
  # names(preds) <- names(ensemble$models)

  weights <- list(length = length(ensemble$models))
  test.set <- GetFactorEqual(test.set)
  i = 0
  for (ind.pred in preds) {
    i = i + 1
    # those tricky SVM's
    if (names(preds)[[i]] == "svm.formula") {
      pred <- GetSVMScale(ind.pred)
      conf.matrix = caret::confusionMatrix(test.set, as.factor(pred))
    } else {
      conf.matrix = caret::confusionMatrix(test.set, as.factor(round(ind.pred[, 1])))
    }
    weights[[i]] = conf.matrix$byClass[train.type][[1]]
  }
  return(weights)
}

#' A helper function used by the Ensemble function. It will take the given inputs
#' and build the weights for each of the models.  This function mostly just handles different
#' cases and error checking and calls another function to get the weights data.
#'
#' @param ensemble The training data to build the models.
#' @param weights Either a given vector of weights, or the value "none".
#' @param test.set The data test set so that weights could be gathered from the models. Not
#' needed if weights are given.
#' @param train.type The metric to be used in getting model weights. Not
#' needed if weights are given.
#'
#' @return A vector containg weights for the Ensemble's models.
#'
#' @examples
#' # This function is used by Ensemble. It's usage is as follows:
#' # ensemble$weight.list = GetModelWeights(ensemble, weights, test.set, train.type)
#'
#' @keywords internal
#' @export
GetModelWeights <- function(ensemble, weights, test.set, train.type) {
  if (class(weights) == "character" && weights == "none") {
    message("Weights were not supplied.  Note that in order to use a weighted voting
            function you must supply them with the GetModelWeights function")
    if (ensemble$.voting.type != "majorityWeight") {
      return(NULL)
    }
  } else {
    # weights are given or voting type is weighting - thus, assign the weights
    if (class(weights) == "data.frame") {
      return(GetWeightsFromTestingSet(ensemble, weights, test.set, train.type))
    } else {
      if (length(weights) == length(ensemble$models)) {
        if (class(weights[[1]]) == "numeric") {
          return(weights)
        } else {
          stop("Weights given are not numeric")
        }
      } else {
        stop("Length of weights do not match number of models")
      }
    }
  }
}


#' Ensemble Creation
#'
#' This function takes models and turns them into a combined Ensemble
#'
#' @param model.list The models to be used in the Ensemble
#' @param voting.type The voting type of the Ensemble (average vote, majority vote,
#'  or majority weight)
#' @param weights A vector of length as long as the model.list containing numeric percent values
#' c(.76, .56, etc.) to be used for a weighted vote Ensemble. For auto calculation, leave
#' this blank and provide a test set.  If the Ensemble is not using majority weighting, then
#' both this field and the test.set field are optional.
#' @param test.set The data test set so that weights could be gathered from the models. Not
#' needed if weights are given.
#' @param train.type The metric to be used in getting model weights. Not
#' needed if weights are given.
#'
#' @return An Ensemble Object
#'
#' @examples
#' # prepare a binary classification dataset
#' iris <- PrepareIris()
#' # create the models
#' comp <- GetModelComparisons(iris[,1:4], iris[,5])
#' # use the models in the comparison to form a one model Ensemble
#' ensem <- Ensemble(comp$model.list, "majorityWeight", iris[,1:4], iris[,5])
#'
#' @export
Ensemble <- function(model.list, voting.type, weights = "none", test.set = "none",
                     train.type = "Balanced Accuracy") {
  ensemble <- list()
  class(ensemble) <- "Ensemble"
  ensemble$.voting.type = voting.type
  ensemble$models <- model.list
  ensemble$weight.list = GetModelWeights(ensemble, weights, test.set, train.type)
  return(ensemble)
}
