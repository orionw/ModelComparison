#source("~/BestModel/R/ModelGeneration.R")

#' This function evalutates many different machine learning models and returns those models with comparison charts
#' @param trainingSet the dataset to be trained on
#' @param trainingClasses the labels of the training set
#' @keywords
#' @export
#' @examples
#' plot()
plot.ModelComparison <- function(object, labels, training_data = "none", predictions="empty") {
  # error check the arguments
  if (class(training_data) != "data.frame" && training_data == "none") {
        if (predictions == "empty") {
      # predictions somehow failed to happen - predict in here
      stop("Both training data and prediction vector were not given.  Give at least one to plot.")
    } else {
      # use the given predictions
      pred_basic = predictions
    }
  } else {
    # check to see if the training_data hasn't been prepped and if it was trained on prepped data
    is_prepped <- sapply(training_data, function(x) (is.numeric(x) || length(levels(x)) <= 2))

    if (object$force_prepared || sum(is_prepped) != ncol(training_data)) {
      # Data is not in one hot encoding - try to do it
      training_data = prepData(training_data)
    }
  }

  if (predictions == "empty") {
    # Predictions not given - create them here from training data
    pred_basic <- predict(object, newdata=training_data, type="prob")
  } else {
    # use the given predictions
    pred_basic = predictions
  }


  if (object$.multi_class == TRUE) {
    # do stuff later
    message("dataset is multi-class")
  } else {
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
