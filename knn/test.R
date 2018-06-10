library(tidyverse)
source("knn/knn")

# Test knn with a fake dataset that contains 2 predictors and a categorical response with 2 levels

data = data.frame(
  x1 = runif(500, min = 0, max = 10),
  x2 = runif(500, min = 0, max = 10)
)

y <- as.factor(ifelse(0.5 * data$x1 + 3 < data$x2, 0, 1))

# Split the dataset into a test dataset that contains 100 observations and a training dataset
# that contains the remaining observations
isTest <- sample(1:nrow(data), 100)

testData <- data[isTest,]
testY <- y[isTest]

trainData <- data[-isTest,]
trainY <- y[-isTest]

# Estimate by cross-validation the best value for k to use
# Default range of k's evaluted is from 1 to 10 over a 10-fold CV
crossValidationResult <- knn.cv(trainData, trainY)

# Plot the resulting error rate in function of K
ggplot(mapping = aes(x = as.numeric(names(crossValidationResult$errorRates)), y = crossValidationResult$errorRates)) +
  geom_path() +
  labs(x = 'K', y = 'test error rate')

# Make predictions on the test dataset
bestK <- crossValidationResult$best
predictions <- knn(testData, trainData, trainY, bestK)

# Plots trainData vs testData
trainData %>% ggplot(mapping = aes(x = x1, y = x2, color = trainY)) +
  geom_point()

testData %>% ggplot(mapping = aes(x = x1, y = x2, color = predictions)) +
  geom_point()

# Test training error rate
sum((predictions != testY)) / length(trainY)


