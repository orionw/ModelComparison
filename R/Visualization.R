#' Plot Comparisons
#'
#' This function evalutates many different machine learning models and returns plots comparing
#' them.
#' @param object The ModelComparison object
#' @param labels The labels of the training set
#' @param training.data The dataset to be trained on. If predictions are provided this is not
#' needed.
#' @param predictions The list of predictions from the models, optional if training.data is not
#' provided.
#' @param plot.type A vector of metrics (as characters) that are the values seen in the plot
#' (examples include ROC, AUC, Accuracy, etc.)  Note: ROC cannot be plotted with other metrics.
#' @param format.data Whether the data should be transformed into one-hot encoding if it needs
#' to be. The default is TRUE. If you would like to predict on unchanged data that is not
#' in the right format, set this to false at your own risk.
#' @export
#' @examples
#' # prepare the dataset
#' titanic <- PrepareNumericTitanic()
#'
#' # create the models
#' comp <- GetModelComparisons(titanic[, -1], titanic[, 1], model.list = "all")
#'
#' # Default.  Plot AUC, Accuracy, Recall, and Precision
#' plot(comp, titanic[, 1], titanic[, -1], plot.type=c("All"))
#'
#' # Choose specific metrics
#' plot(comp, titanic[, 1], titanic[, -1], plot.type=c("Specificity", "Precision", "AUC",
#' "Recall", "Detection Rate"))
#'
#' # plot overlapping ROC lines
#' plot(comp, titanic[, 1], titanic[, -1], plot.type="roc")
plot.ModelComparison <- function(object, labels, training.data = "none", predictions="empty",
                                 plot.type="ROC", format.data=TRUE, ...) {
  # error check the arguments
  if (class(training.data) != "data.frame" && training.data == "none") {
        if (predictions == "empty") {
      # predictions somehow failed to happen - predict in here
      stop("Both training data and prediction vector were not given.  Give at least one to plot.")
    } else {
      # use the given predictions
      pred.basic = predictions
    }
  } else {
    # check to see if the training.data hasn't been prepped and if it was trained on prepped data
    is.prepped <- sapply(training.data, function(x) (is.numeric(x) || length(levels(x)) <= 2))

    if ((object$force.prepared || sum(is.prepped) != ncol(training.data)) && format.data) {
      # Data is not in one hot encoding - try to do it
      training.data = prepData(training.data)
    }
  }

  if (predictions == "empty") {
    # Predictions not given - create them here from training data
    pred.basic <- predict(object, newdata=training.data, type="prob")
  } else {
    # use the given predictions
    pred.basic = predictions
  }

  ## Plot the chosen type
  if (class(plot.type) == "character" || class(plot.type) == "list") {
    plot.type = sapply(plot.type, tolower)
    if (length(plot.type) == 1 && plot.type == c("roc")) {
      CreateROCPlot(object, pred.basic, labels)
    } else if (class(plot.type) == "character") {
      if (sum(is.element(plot.type, "all"))) {
        CreateCombinedPlot(object, pred.basic, labels)
      } else {
        simpleCap <- function(x) {
          if (x == "auc") {
            return(toupper(x))
          }
          s <- strsplit(x, " ")[[1]]
          return(paste(toupper(substring(s, 1,1)), substring(s, 2),
                sep="", collapse=" "))
        }
        plot.type <- sapply(plot.type, simpleCap)
        CreateCombinedPlot(object, pred.basic, labels, plot.type)
      }
    }
  } else {
    stop("Undefined plot.type.  Please check the documentation.")
  }
}

#' A helper function used by the plot.ModelComparison.  This function gathers the data needed
#' for the ROC lines and then plots the chart.
#'
#' @param object The ModelComparison object.
#' @param pred.basic The predictions from the models on the testing data.
#' @param labels The labels for the testing data.
#'
#' @return NULL
#'
#' @examples
#' # This function is used by plot.ModelComparison. It's usage is as follows:
#'  CreateROCPlot(object, pred.basic, labels)
#' @keywords internal
#' @export
CreateROCPlot <- function(object, pred.basic, labels) {
  if (object$.multi.class == TRUE) {
    # do stuff later TODO
    message("dataset is multi-class")
  } else {
    i = 0
    colorPal = rainbow(length(object$model.list))
    for (model in object$model.list) {
      i = i + 1
      if (!is.null(model)) {
        # if given in dataframe format, reduce to vector
        if (class(pred.basic[[i]]) == "data.frame" || class(pred.basic[[i]]) == "matrix") {
          pred.basic[[i]] <- pred.basic[[i]][, 1]
        }
        if (i == 1) {
          # do this to init the plot - for the first model
          assertthat::are_equal(length(labels), length(pred.basic[[i]]))
          roc.plot <- pROC::roc(labels, pred.basic[[i]])
          plot(roc.plot, col = colorPal[i], title="ROC Comparison")
        } else {
          assertthat::are_equal(length(labels), length(pred.basic[[i]]))
          roc.plot <- pROC::roc(labels, pred.basic[[i]])
          plot(roc.plot, add = T, col = colorPal[i])
        }
      }
      legend("topright", legend=names(object$model.list),
             col=colorPal, lty=1:2, cex=0.8)
    }
  }
}

#' A helper function used by the plot.ModelComparison.  This function gathers the data needed
#' for many types of metrics and then plots the chart.
#'
#' @param object The ModelComparison object.
#' @param pred.basic The predictions from the models on the testing data.
#' @param labels The labels for the testing data.
#'
#' @return NULL
#'
#' @examples
#' # This function is used by plot.ModelComparison. It's usage is as follows:
#'  CreateCombinedPlot(object, pred.basic, labels)
#'  # or given specific metrics for Accuracy and AUC:
#'  CreateCombinedPlot(object, pred.basic, labels, c("Accuracy", "AUC"))
#' @keywords internal
#' @export
CreateCombinedPlot <- function(object, pred.basic, labels,
                               metrics.for.plot = c("Accuracy", "Recall", "AUC", "Precision")) {
  metric.list = list()
  # go through and create combined graph for Accuracy, Recall, AUC, and Precision
  metric.count = 0
  for (metric in metrics.for.plot) {
    # reset the values every run
    metric.count = metric.count + 1
    value.list = list()
    i = 0
    for (ind.pred in pred.basic) {
      i = i + 1
      if (metric == "AUC") {
        # calculate the AUC (grab the first item which is the numeric part)
        if (names(pred.basic)[[i]] == "svm.formula") {
          # scale the values between 1 and 0
          preProcValues <- caret::preProcess(ind.pred, method = "range")
          dataScaled <- predict(preProcValues, ind.pred)
          # remove the matrix form
          ind.pred <- c(dataScaled)
          value.list[[i]] = pROC:::auc(labels, ind.pred)[[1]]
        } else {
          value.list[[i]] = pROC:::auc(labels, ind.pred[, 1])[[1]]
        }
      } else {
        if (names(pred.basic)[[i]] == "svm.formula") {
          pred <- GetSVMScale(ind.pred)
        } else {
          pred <- as.factor(round(ind.pred[, 1]))
        }
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
  metric.df <- data.frame(t(data.frame(matrix(unlist(metric.list), nrow=length(metric.list),
                                              byrow=T), stringsAsFactors=FALSE)))
  colnames(metric.df) <- metrics.for.plot

  # add model types for GGplot2 and get it into the right form
  metric.df["model"] <- names(object$model.list)
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
  #         ylim=c(0, 1), ylab='Percentage', col=rainbow(length(object$model.list)),
  #         xlab="Model Name", main="Model Comparison", beside = TRUE)
  #
  # legend(par('usr')[2], 1, title="Model Type", legend=names(object$model.list),
  #        col=rainbow(length(object$model.list)),lty=1, cex=0.8)
}

#' A helper function to ground SVM predictions
#'
#' SVM predictions are often greater than 1 or less than 0. We need to round them to make them
#' have only two factors
#'
#' @param pred The SVM predictions vector.
#'
#' @return The predictions, with only two factors.
#'
#' @examples
#' As used in GetCombinedPlot:
#' pred <- GetSVMScale(pred)
#'
#' @keywords internal
#' @export
GetSVMScale <- function(pred) {
  pred <- c(pred)
  if (length(levels(as.factor(round(pred)))) > 2) {
    pred <- ifelse(pred < 0, 0, pred)
    pred <- ifelse(pred > 0, 1, pred)
  }
  return(pred)

}

