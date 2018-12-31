# helper functions to get predictions
StripPredictions <- function(pred) {
  message("In Strip Predictions")
  message(class(pred))
  message("Pred is done")
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

predict.Ensemble <- function(object, testdata, voting_type="default") {
  print("In Ensemble Predict function")
  message("In Ensemble Predict function")

  preds <- list(length=length(object$models))
  # predict on list of models
  i = 0
  for (model in object$models) {
    i = i + 1
    preds[[i]] <-  StripPredictions(predict(model, newdata = testdata, type="prob"))
  }

    # input overrides set voting type
  if (voting_type != "default") {
      if (voting_type == "majorityVote") {
        return(MajorityVote(preds))
    } else if (voting_type == "majorityWeight") {
      return(as.factor((MajorityWeight(preds, object$weight.list))))
    } else if (voting_type == "averageVote") {
      return(as.factor((AverageVote(preds))))
    } else {
      stop("Voting type was not correctly specified.  Please use 'majorityVote',
           'majorityWeight', or 'averageVote'")
    }
  } else if (object$.voting_type == "majorityVote") {
    # round the values before passing them in
    return(as.factor(MajorityVote(round(preds))))

  } else if (object$.voting_type == "majorityWeight") {
    # not majority vote, use weighting
    return(as.factor((MajorityWeight(preds, object$weight.list))))

  } else if (object$.voting_type == "averageVote"){
      # use average weighting
    return(as.factor((AverageVote(preds))))
  }
}


MajorityVote <- function(list_of_predictions){
  if (anyNA(list_of_predictions, recursive = TRUE)) {
    stop("There are NA's in this prediction.  Please predict correct classes")
  }
  votesFinal <- list(length=length(list_of_predictions[[1]]))
  for(i in 1:length(list_of_predictions[[1]])){
    # for length of the predictions
    vote = vector(mode="numeric", length=length(list_of_predictions))
    j = 0
    for (model_pred in list_of_predictions) {
      j = j + 1
      # for each model at that prediction index
      vote[[j]] = round(model_pred[[i]])
    }
    votesFinal[[i]] = (names(which.max(table(vote))))
  }
  message(votesFinal)
  message((as.list(as.numeric(votesFinal))))
  return((as.numeric(votesFinal)))
}

MajorityWeight <- function(list_of_predictions, weight_list){
  if (anyNA(list_of_predictions, recursive = TRUE)) {
    stop("There are NA's in this prediction.  Please predict correct classes")
  }
  votesFinal <- list(length=length(list_of_predictions[[1]]))
  for(i in 1:length(list_of_predictions[[1]])){
    # for length of the predictions
    vote = vector(mode="numeric", length=length(list_of_predictions))
    j = 0
    for (model_pred in list_of_predictions) {
      j = j + 1
      # for each model at that prediction index
      vote[[j]] = model_pred[[i]] * weight_list[[j]]
    }
    sum_preds = sum(vote)
    if (sum_preds > 1) {
      sum_preds = 1
    } else if (sum_preds < 0) {
      sum_preds = 0
    }
    votesFinal[[i]] = sum_preds
  }
  # remove all names associated
  names(votesFinal) <- c()
  return(as.list(votesFinal))
}

AverageVote <- function(list_of_predictions){
  if (anyNA(list_of_predictions, recursive = TRUE)) {
    stop("There are NA's in this prediction.  Please predict correct classes")
  }
  votesFinal <- list(length=length(list_of_predictions[[1]]))
  for(i in 1:length(list_of_predictions[[1]])){
    # for length of the predictions
    vote = vector(mode="numeric", length=length(list_of_predictions))
    j = 0
    for (model_pred in list_of_predictions) {
      j = j + 1
      # for each model at that prediction index
      vote[[j]] = model_pred[[i]]
    }
    sum_preds = sum(vote) / length(list_of_predictions)
    if (sum_preds > 1) {
      sum_preds = 1
    } else if (sum_preds < 0) {
      sum_preds = 0
    }
    votesFinal[[i]] = sum_preds
  }
  # remove all names associated
  names(votesFinal) <- c()
  return(as.list(votesFinal))
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
  for (ind_pred in preds) {
    i = i + 1
    conf.matrix = confusionMatrix(test.set, as.factor(round(ind_pred[, 1])))
    weights[[i]] = conf.matrix$byClass[train.type][[1]]
  }
  return(weights)
}

GetModelWeights <- function(ensemble, weights, test.set, train.type) {
  if (class(weights) == "character" && weights == "none") {
    message("Weights were not supplied.  Note that in order to use a weighted voting
            function you must supply them with the GetModelWeights function")
    if (ensemble$.voting_type != "majorityWeight") {
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
Ensemble <- function(model_list, voting_type, weights = "none", test.set = "none", train.type = "Balanced Accuracy") {
  ensemble <- list()
  class(ensemble) <- "Ensemble"
  ensemble$.voting_type = voting_type
  ensemble$models <- model_list
  ensemble$weight.list = GetModelWeights(ensemble, weights, test.set, train.type)
  return(ensemble)
}
