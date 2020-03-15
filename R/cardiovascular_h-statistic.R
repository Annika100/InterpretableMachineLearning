# install packages
install.packages("iml")
install.packages("mlr")
install.packages("ggplot2")
install.packages("e1071")
install.packages("devtools")


library('mlr')
library('iml')
library("ggplot2")


# read data
cardiovascular <- read.csv("C:/Users/Annika Fischer/Desktop/Uni/Master_Informationsverarbeitung/19-20_WiSe/Interpretable_Machine_Learning/Hausarbeit/Daten/cardiovascular-disease-dataset/cardiovascular.csv")

# remove "id" column from dataset
cardiovascular = cardiovascular[,c(2,3,4,5,6,7,8,9,10,11,12,13)]
write.csv(cardiovascular, file="cardiovascular.csv")

# view dataset
#View(cardiovascular)

# train model for a classification problem with a random forest for predicting cardiovascular
cardiovascular.task = makeClassifTask(data = cardiovascular, target = "cardio")
mod.cardiovascular = mlr::train(mlr::makeLearner(cl = 'classif.randomForest', id = 'cardiovascular-rf', predict.type = 'prob'), cardiovascular.task)



### Feature Interaction

# calculate feature interaction (h-statistic) of every feature with all other features
pred.cardiovascular = Predictor$new(mod.cardiovascular, data = cardiovascular[setdiff(colnames(cardiovascular), "cardio")])
ia = Interaction$new(pred.cardiovascular, grid.size = 50) 
# plot
plot(ia)



# calculate 2-way feature interaction (H-statistic) of one feature with all other features
ia2 = Interaction$new(pred.cardiovascular, grid.size = 50, feature = "age") 
# plot 
plot(ia2)

