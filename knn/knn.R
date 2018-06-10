#' Attempt of implementation of the k-nearest neighbors algorithm
#' @author Vincent Brouté

library(tidyverse)

#' Perform a k-nearest neighbors to predict response for x
#' from training observations data and responses y
#' @param x data.frame the observations to predict
#' @param data data.frame the training set of observations
#' @param y vector the responses for the related training observation
#' @param k The number of neighbors to use for predictions
knn <- function(x, data, y, k)
{
  x <- as.matrix(x)
  data <- as.matrix(data)
  
  result <- factor(rep(levels(y)[0], nrow(x)), levels = levels(y))
  
  for (row in 1:nrow(x)) {
    
    # Compute the K nearest neighbors for each observation to predict the response
    distances <- rep(0, nrow(data))
    
    for(i in 1:nrow(data)) {
      for (j in 1:ncol(data)) {
        distances[i] <- distances[i] + (data[i,j] - x[row,j]) ^ 2
      }
    }
    
    nearestObservationsIndexes <- sort(distances, index.return = T)$ix[1:k]
    nearestResponses <- y[nearestObservationsIndexes]
    
    # If y is a categorical response, we take as response the most frequent response among the
    # k nearest neighbors. Otherwise, for quantitative response, we compute the mean response of the knn
    if(is.factor(y)) {
      highestVote <- data.frame(y = nearestResponses) %>%
        group_by(y) %>%
        summarize(n = n()) %>%
        arrange(desc(n))
      
      result[row] <- highestVote$y[1]
    } else {
      result[row] <- mean(nearestResponses)
    }
  }
  
  result
}

#' Choose the best value for K, the number of neighbors to use for KNN
#' By performing cross-validation. Default is a 10-fold cross validation for K from 1 to 10.
#' @param data data.frame the training set of observations
#' @param y vector the responses for the related training observation
#' @param kRange The range of values for k to evalute
#' @param foldsNb Number of folds to use for performing the cross validation
knn.cv <- function(data, y, kRange = 1:10, foldsNb = 10)
{
  data <- as.matrix(data)
  
  # Split the data row indexes into foldsNb folds
  rows <- sample(1:nrow(data))
  folds <- split(rows, ceiling(1:nrow(data) * (foldsNb / nrow(data)) ))
  
  # Evaluate test error rate (in case of categorical variable) or test MSE (for quantitative variable)
  # For each k from kRange
  errors <- rep(0, length(kRange))
  names(errors) <- kRange
  for (i in 1:length(folds)) {
    
    trainData <- data[-folds[[i]],]
    testData <- data[folds[[i]],]
    testY <- y[folds[[i]]]
    
    for (k in kRange) {
      predictions <- knn(testData, trainData, y[-folds[[i]]], k)
      
      # Sum RSS or the number of errors depending if the response is categorical or not
      if (is.factor(testY)) {
        errors[k] <- errors[k] + sum(predictions != testY)
      } else {
        errors[k] <- errors[k] + sum((predictions - testY)^2)
      }
    }
  }
  
  # Dividing by nrow(data) the RSS or the number of errors allows to get the test error rate pour test MSE
  list(
    errorRates = errors / nrow(data),
    best = which.min(errors / nrow(data))
  )
}