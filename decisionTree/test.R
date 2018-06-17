library(tidyverse)
source("decisionTree/decisionTree.R")

# Test decision tree regression with a fake dataset taht contains only one predictor and a reponse

# generate sample dataset
x <- sample(seq(from = -20, to = 20, by = 0.3))
y <- 5 * x + 0.5 * x^2 - 0.1 * x^3 + rnorm(length(x), sd = 180)

data <- data.frame(x, y)

tree <- decisionTree(data)

printTree(tree)

testObservations <- data.frame(x = seq(from = -20, to = 20, by = 0.5))
predictions <- predictFromTree(tree, testObservations)

ggplot(mapping = aes(x, y)) +
  geom_point() +
  geom_path(mapping = aes(testObservations['x'], y = predictions[,'y']), color = "red")