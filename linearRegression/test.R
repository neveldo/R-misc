library(tidyverse)
source("linearRegression/linearRegression.R")

# Test linear regression with a fake dataset taht contains only one predictor and a reponse
# With an exponential shape

x <- seq(from = 0, to = 10, by = 0.1)
y <- 2^x + rnorm(length(x), sd = 120)

# Compute coefficients, get statistical indicators and plot the predicted responses
coefs <- linearRegression(2^x, y)

linearRegression.indicators(2^x, y, coefs)

predictions <- linearRegression.predict(2^x, coefs)

ggplot(mapping = aes(x, y)) +
  geom_point() +
  geom_path(mapping = aes(y = predictions), color = "red")
