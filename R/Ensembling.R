# helper functions to get predictions
StripPredictions <- function(pred) {
  if (class(pred) == "list") {
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

predict.Ensemble <- function(object, newdata, voting.type="default", ...) {
  preds <- list(length=length(object$models))
  # predict on list of models
  i = 0
  for (model in object$models) {
    i = i + 1
    preds[[i]] <-  StripPredictions(predict(model, newdata = newdata, type="prob"))
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


GetWeightsFromTestingSet <- function(ensemble, df.train, test.set, train.type) {
  i = 0
  preds = list(length = length(ensemble$models))
  for (model in ensemble$models) {
    i = i + 1
    preds[[i]] <- predict(model, df.train, type="prob")
  }
  weights <- list(length = length(ensemble$models))
  test.set <- GetFactorEqual(test.set)
  i = 0
  for (ind.pred in preds) {
    i = i + 1
    conf.matrix = caret::confusionMatrix(test.set, as.factor(round(ind.pred[, 1])))
    weights[[i]] = conf.matrix$byClass[train.type][[1]]
  }
  return(weights)
}

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


## weights are either a list of numeric values, or a dataframe that will be used as
## a training set.
Ensemble <- function(model.list, voting.type, weights = "none", test.set = "none", train.type = "Balanced Accuracy") {
  ensemble <- list()
  class(ensemble) <- "Ensemble"
  ensemble$.voting.type = voting.type
  ensemble$models <- model.list
  ensemble$weight.list = GetModelWeights(ensemble, weights, test.set, train.type)
  # TODO add accuracy for ensembles to pull out
  return(ensemble)
}
