library(tidyverse)
library(MASS)

source("randomForest/randomForest.R")

# Test random forest with the Boston dataset from MASS

nrow(Boston)

isTest <- sample(1:nrow(Boston), 100)
bostonTest <- Boston[isTest,]
bostonTrain <- Boston[-isTest,]

randomForest <- randomForest(bostonTrain, ceiling(sqrt(ncol(Boston) - 1)), 20)

predictions <- predictFromRandomForest(randomForest, bostonTest)

rmse <- sqrt(sum((predictions$y - mean(bostonTest$medv))^2) / nrow(bostonTest))

rmse