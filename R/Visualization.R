#' This function evalutates many different machine learning models and returns those models with comparison charts
#' @param trainingSet the dataset to be trained on
#' @param trainingClasses the labels of the training set
#' @param plot.type A vector of characters that are the values seen in the plot
#' (examples include ROC, AUC, Accuracy, etc.)  Note: ROC cannot be plotted with other metrics.
#' @keywords
#' @export
#' @examples
#' plot()
plot.ModelComparison <- function(object, labels, training_data = "none", predictions="empty", plot.type="ROC", ...) {
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

  ## Plot the chosen type
  if (length(plot.type) == 1 && plot.type == c("ROC")) {
    CreateROCPlot(object, pred_basic, labels)
  } else if (class(plot.type) == "character") {
    if (sum(is.element(plot.type, "All"))) {
      CreateCombinedPlot(object, pred_basic, labels)
    } else {
      CreateCombinedPlot(object, pred_basic, labels, plot.type)
    }
  } else {
    stop("Undefined plot.type.  Please check the documentation.")
  }
}


CreateROCPlot <- function(object, pred_basic, labels) {
  if (object$.multi_class == TRUE) {
    # do stuff later TODO
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


CreateCombinedPlot <- function(object, pred_basic, labels,
                               metrics.for.plot = c("Accuracy", "Recall", "AUC", "Precision")) {
  metric.list = list()
  # go through and create combined graph for Accuracy, Recall, AUC, and Precision
  metric.count = 0
  for (metric in metrics.for.plot) {
    # reset the values every run
    metric.count = metric.count + 1
    value.list = list()
    i = 0
    for (ind_pred in pred_basic) {
      i = i + 1
      if (metric == "AUC") {
        # calculate the AUC (grab the first item which is the numeric part)
        value.list[[i]] = pROC:::auc(labels, ind_pred[, 1])[[1]]
      } else {
        pred <- as.factor(round(ind_pred[, 1]))
        # predictions come out backwords, flip them
        pred <- GetFactorEqual(pred)
        # assign the correct labels
        levels(pred) <- levels(labels)
        conf.matrix = caret::confusionMatrix(labels, pred)
        # grab the statistic
        if (metric == "Accuracy") {
          value.list[[i]] = conf.matrix$overall[metric]
        } else {
          # all other metrics
        value.list[[i]] = conf.matrix$byClass[metric]
        }
      }
      if (is.na(value.list[[1]])) {
        stop("plot.type is not a valid metric name. Please see the documentation for details.")
      }
    }
    metric.list[[metric.count]] = value.list
  }
  # turn list of lists into a dataframe
  metric.df <- data.frame(t(data.frame(matrix(unlist(metric.list), nrow=length(metric.list), byrow=T),
                          stringsAsFactors=FALSE)))
  colnames(metric.df) <- metrics.for.plot

  # add model types for GGplot2 and get it into the right form
  metric.df["model"] <- names(object$model_list)
  data.m <- reshape2::melt(metric.df, id.vars='model')

  # Plot with GGplot2
  ggplot(data.m, aes(model, value)) + geom_bar(aes(fill = variable),
    width = 0.4, position = position_dodge(width=0.5), stat="identity") +
    labs(title= "Model Comparison",
         y="Percent", x = "Model", fill = "Metric")

  # # to let the legend grow
  # par(oma=c(0, 0, 0, 5))
  # # Plot with Base R, by removing the model column and using a matrix
  # barplot(as.matrix(metric.df[, -dim(metric.df)[[2]]]), names.arg=names(metrics.for.plot),
  #         ylim=c(0, 1), ylab='Percentage', col=rainbow(length(object$model_list)),
  #         xlab="Model Name", main="Model Comparison", beside = TRUE)
  #
  # legend(par('usr')[2], 1, title="Model Type", legend=names(object$model_list),
  #        col=rainbow(length(object$model_list)),lty=1, cex=0.8)
}
