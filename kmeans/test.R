library(tidyverse)
source("kmeans/kmeans.R")

# Test K-means clustering on a fake dataset

# Generate a dataset with 3 fake clusters
data <- bind_rows(
  data.frame(a = rnorm(100, -1), b = rnorm(100, 1)),
  data.frame(a = rnorm(100, 2.8), b = rnorm(100, 3)),
  data.frame(a = rnorm(100, 2.5), b = rnorm(100, -2))
)

# Apply K-means algorithm to find the 3 clusters
clusters <- kmeans(data, 3)

# scatterplot before/after clustering
data %>% 
  ggplot(mapping = aes(x = a, y = b)) +
  geom_point()

bind_cols(
  data, 
  cluster = as.factor(clusters)
) %>% 
  ggplot(mapping = aes(x = a, y = b, color = cluster)) +
  geom_point()