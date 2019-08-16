<!-- badges: start -->
[![Travis build status](https://travis-ci.org/orionw/ModelComparison.svg?branch=master)](https://travis-ci.org/orionw/ModelComparison)
[![codecov](https://codecov.io/gh/orionw/ModelComparison/branch/master/graph/badge.svg)](https://codecov.io/gh/orionw/ModelComparison)
  <!-- badges: end -->
# ModelComparison
An R Package to return and compare variety of different model types, complete with hyper-parameter tuning options

Install by using `install_github("orionw/ModelComparison")` after installing and loading the `devtools` library

Example usage:
```R
library(ModelComparison)
# prepare the dataset.  This function creates a two class Iris dataset.
iris_data <- PrepareIris()

# create the models.  This includes SVM's, K-NN, A 4 layer Neural Network, and Linear or Logistic Regression.
comp <- GetModelComparisons(iris_data[, -5], iris_data[, 5], model.list = "all")

# get prediction values for the models
preds = predict(comp, newdata = iris_data[, -5], type="prob")

# Default.  Plot AUC, Accuracy, Recall, and Precision
plot(comp, iris_data[, 5], predictions=preds, plot.type=c("All"))

# Choose specific metrics
plot(comp, iris_data[, 5], predictions=preds, plot.type=c("Specificity", "Precision", "AUC", "Recall", "Detection Rate"))

# plot overlapping ROC lines from all models
plot(comp, iris_data[, 5], predictions=preds, plot.type="roc")

```
