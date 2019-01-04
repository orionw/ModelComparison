GetPredictionsForStacking <- function(models, x.values) {
  i = length(colnames(x.values))

  names.df <- colnames(x.values)
  for (count in range(1, length(models))) {
    names.df[[count + i]] <- names(models)[[count]]
  }
  for (model in models) {
      i = i + 1
      df <-  StripPredictions(predict(model, newdata = x.values, type="prob"))
      x.values <- cbind(x.values, df)
  }
  colnames(x.values) <- names.df
  return(x.values)
}

PrepareIris <- function() {
  data(iris)
  toBeRemoved<-which(iris$Species=="setosa")
  irisReal <-iris[-toBeRemoved,]
  irisReal <- droplevels(irisReal)
  levels(irisReal$Species) <- c('versicolor', 'virginica' )
  return(irisReal)
}
