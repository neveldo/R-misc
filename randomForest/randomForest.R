#' Attempt of implementation of Random Forest for Regression
#' @author Vincent Brouté

source("decisionTree/decisionTree.R")

#' Build a randon forest
#' @param data.frame data ll column except the last one are assumed to be predictors variables. The
#' last one is assumed to be the quantitative response variable
#' @param int m number of candidate predictors to consider at each split. For instance (for instance : qrt(nbPredictors))
#' @param int treesNumber the number of trees to grow
#' @param int maxDepth The max depth until stopping to grow the tree
#' @param int minNodeRecords The minimum number of records allowed in a node until stopping to grow the tree
#' @return list list of trees built from decisionTree() calls
randomForest <- function(data, m, treesNumber = 100, maxDepth = 10, minNodeRecords = 3)
{
  result <- vector("list", treesNumber) 
  for (i in 1:treesNumber) {
    # Generate a new bootstrap sample from the original data, for each tree to grow
    sampleRows <- sample(rownames(data), nrow(data), replace = T)
    sampleDataset <- data[sampleRows,]
    result[[i]] <- decisionTree(data, maxDepth, minNodeRecords, m)
  }
  result
}

#' Predict a quantitative response for observations with randomForest
#' @param list randomForest tree to use for prediction, built with randomForest()
#' @param data.frame observations The observations to use for prediction
#' @param boolean raw Set to TRUE in order to return a matrix for each tree from the random forest 
predictFromRandomForest <- function(randomForest, observations, raw = FALSE)
{
  predictions <- matrix(nrow = nrow(observations), ncol = length(randomForest))
  for (i in 1:length(randomForest)) {
    predictions[,i] <- predictFromTree(randomForest[[i]], observations)[,1]
  }
  
  if (raw) {
    predictions
  } else {
    data.frame(y = apply(predictions, 1, mean))
  }
}