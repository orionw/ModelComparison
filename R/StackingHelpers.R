#' A helper function to get predictions added to a dataset.
#'
#' @param models The models that will add their predictions to the DataFrame.
#' @param x.values The DataFrame that will be added on too and predicted on.
#'
#' @return The given DataFrame (x.values) with an additional column of predictions for each
#' model given.
#'
#' @examples
#' iris <- PrepareIris()
#' x.values = iris[, 1:4]
#' # create the models
#' comp <- GetModelComparisons(x.values, iris[,5])
#' # get the new dataframe
#' df.for.stacking <- GetPredictionsForStacking(comp$model.list, x.values)
#'
#' @keywords internal
#' @export
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

#' A quick helper function to turn the iris dataset into a 2 level dataset.
#'
#' @return The Iris dataset, without the setosa rows
#'
#' @examples
#' iris <- PrepareIris()
#'
#' @keywords internal
#' @export
PrepareIris <- function() {
  data(iris)
  toBeRemoved<-which(iris$Species=="setosa")
  irisReal <-iris[-toBeRemoved,]
  irisReal <- droplevels(irisReal)
  levels(irisReal$Species) <- c('versicolor', 'virginica' )
  return(irisReal)
}

#' A quick helper function to turn the titanic dataset into a numeric only dataset.
#'
#' @return The Titanic dataset, but with only numeric columns.
#'
#' @examples
#' titanic <- PrepareNumericTitanic()
#'
#' @keywords internal
#' @export
PrepareNumericTitanic <- function() {

  print("Listing dirs")
  print(list.files(getwd()))
  titanic <- read.csv("titanic.csv")
  titanic <- titanic[, c("Survived", "Age",
                         "Siblings.Spouses.Aboard", "Parents.Children.Aboard", "Fare")]
  titanic$Survived = as.factor(titanic$Survived)
  levels(titanic$Survived) <- c("died", "survived")
  return(titanic)
}
