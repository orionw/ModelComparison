predict.ensemble <- function(object, testdata, voting_type="default") {

  # predict on list of models
  preds <- predict(object$models, newdata = testdata, type="raw")

  if (voting_type == "default" && object$.voting_type == "majorityVote") {
    # round the values
    return(as.factor(majority_vote(round(preds))))

    # not majority vote, use weighting
  } else {
    return(as.factor(round(majority_weight(preds, object$weight_list))))
  }
}


majority_vote <- function(list_of_predictions){
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
    votesFinal[[i]] = (names(which.max(table(vote))))
  }
  return(as.list(as.numeric(votesFinal)))
}

majority_weight <- function(list_of_predictions, weight_list){
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
  print(votesFinal)
  return(as.list(votesFinal))
}

Ensemble <- function(model_list, voting_type) {
  ensemble <- list()
  class(ensemble) <- "Ensemble"
  ensemble$.voting_type = voting_type
  ensemble$models <- model_list
  return(ensemble)
}
