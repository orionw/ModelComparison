
library(plyr)
library(ggplot2)
library(BestModel)
data(iris)
toBeRemoved<-which(iris$Species=="setosa")
irisReal <-iris[-toBeRemoved,]
count(irisReal$Species)
irisReal <- droplevels(irisReal)
levels(irisReal$Species) <- c('versicolor', 'virginica' )
str(irisReal)
a <- getModelComparisons(irisReal[,1:4], irisReal[,5])
class(a)
a$svmLinear
a$neuralNet$neuralNet$finalModel
pred_list <- predict(a, irisReal)
plot(a, irisReal$Species, F)
unlockEnvironment <- function (env) {
  return (new.env(parent=env))
}

e <- unlockEnvironment(environment())
class(a)
# Read in the data with questions
Day0Data = read.table("C:/Users/orion/Desktop/OARS-project/Data/Day0UDT_with_questions.csv", sep=",", header=TRUE)
Month6Test = read.table("C:/Users/orion/Desktop/OARS-project/Data/M6UDT_with_questions.csv", sep=",", header=TRUE)


