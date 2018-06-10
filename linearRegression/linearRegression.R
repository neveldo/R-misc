#' Attempt of implementation of multiple linear regression
#' @author Vincent Brouté

#' Return a vector that contain the result intercept and coefficients for each predictor
#' @see https://economictheoryblog.com/2015/02/19/ols_estimator/
#' @param x A data.frame or matrix of n predictors
#' @param y The response variable
#' @return A vector of coefficients 
linearRegression <- function(x, y)
{
  x <- as.matrix(x)
  y <- as.matrix(y)
  x <- cbind(Intercept = rep(1, nrow(x)), x)
  
  solve(t(x) %*% x) %*% t(x) %*% y
}

#' Predict the response for the given observation(s)
#' @param x A data.frame of observation to make predictions on
#' @param coefs The coefficients to use
#' @return a matrix that contain a single column of predicted values
linearRegression.predict <- function(x, coefs)
{
  x <- as.matrix(x)
  as.matrix(cbind(x0 = rep(1, length(x)), x)) %*% coefs
}

#' Return a list of statistical indicators for a given datasets
#' and set of coefficients :
#' $RSS : Residual Sum of Squares
#' $TSS : Total Sum of Squares
#' $R2 : Coefficient of determination
#' $RSE : Residuals standard error
#' $MSE : Mean Squarred Error
#' For each coefficient :
#' Standard Error
#' T-value and related P-Value
#' @param x A data.frame of observation to make predictions on
#' @param y The response variable
#' @param coefs The coefficients to use
linearRegression.indicators <- function(x, y, coefs)
{
  x <- as.matrix(x)
  predictions <- linearRegression.predict(x, coefs)
  
  RSS <- sum((y - predictions)^2)
  TSS <- sum((y - mean(y))^2)
  R2 <- (TSS-RSS) / TSS
  RSE <- sqrt(RSS / (nrow(predictions) - 2))
  MSE <- RSS / nrow(predictions)
  
  # Compute the standard error for each coefficient using the boostrap approach
  # by generating 1000 samples datasets within replacements
  bootstrapSamplesNb <- 1000
  bootstrapCoefs <- matrix(ncol = ncol(x) + 1, nrow = bootstrapSamplesNb)
  for (i in 1:bootstrapSamplesNb) {
    sampleRows <- sample(nrow(x), nrow(x), replace = T)
    sampleX <- x[sampleRows,]
    sampleCoefs <- linearRegression(sampleX, y)
    bootstrapCoefs[i,] <- sampleCoefs
  }
  
  coefsStandardError <- apply(bootstrapCoefs, 2, function(col) {
    sqrt(sum((col - mean(col))^2) / length(col))
  })
  
  coefsTValue <- coefs / coefsStandardError
  
  # The p-value follows a student t distribution
  coefsPValue <- 2 * pt(abs(coefsTValue), nrow(x) - 2, lower=FALSE)
  
  coefs <- cbind(coefsStandardError, as.vector(coefsTValue), as.vector(coefsPValue))
  rownames(coefs) <- rownames(coefsTValue)
  colnames(coefs) <- c("Standard Error", "T-Value", "P-Value")
  
  list(RSS = RSS, TSS = TSS, R2 = R2, RSE = RSE, coefs = coefs)
}