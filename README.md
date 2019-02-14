# BestModel
An R Package to return and compare variety of different model types, complete with hyper-parameter tuning options

Install by using `install_github("orionw/BestModel")` from the `devtools` library

Publication in CRAN forthcoming

Example usage:
```R
library(BestModel)
# prepare the dataset.  This function provided cleans the Titanic dataset.
titanic <- PrepareTitanic()

# create the models.  This includes SVM's, K-NN, A 4 layer Neural Network, and Linear or Logistic Regression.
comp <- GetModelComparisons(titanic[, -1], titanic[, 1], model.list = "all")

# Default.  Plot AUC, Accuracy, Recall, and Precision
plot(comp, titanic[, 1], titanic[, -1], plot.type=c("All"))

# Choose specific metrics
plot(comp, titanic[, 1], titanic[, -1], plot.type=c("Specificity", "Precision", "AUC", "Recall", "Detection Rate"))

# plot overlapping ROC lines from all models
plot(comp, titanic[, 1], titanic[, -1], plot.type="roc")

```
