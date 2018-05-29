#' Attempt of implementation of the K-means algorithm 
#' based on the description from the book "An Introduction to Statistical Learning"
#' @author Vincent Brouté

#' Return the centroids for each cluster computed from the data
#' @param data data.frame The dataset from which compute the centroids
#' @param clusters vector The current clusters assigned to each row of data
#' @return A list of vectors for the centroids of the k clusters
getCentroids <- function(data, clusters)
{
  k <- max(clusters)
  centroids <- vector("list", k)
  
  for (cluster in 1:k) {
    clusterSubset <- data[clusters == cluster,]
    
    clusterCentroids = rep(NA, ncol(data))
    for (i in 1:ncol(data)) {
      clusterCentroids[i] = mean(clusterSubset[,i])
    }
    centroids[[cluster]] = clusterCentroids
  }
  centroids
}

#' Compute the squared euclidean distance between the ith observation from the dataset and
#' each cluster centroids, and return the nearest centroid
#' @param data data.frame The dataset
#' @param rowNumber int The row number from the dataset from which compute the distances
#' @return int The nearest cluster for the ith observation
getNearestCluster <- function(data, rowNumber, clusterCentroids)
{
  k <- length(clusterCentroids)
  distances <- rep(0, k)
  for (cluster in 1:k) {
    for (j in 1:ncol(data)) {
      distances[cluster] <- distances[cluster] + (data[rowNumber,j] - clusterCentroids[[cluster]][j]) ^ 2
    }
  }
  which.min(distances)
}

#' Find k clusters in the dataset
#' @param data data.frame The dataset
#' @param k int The number of clusters to find
#' @return vector that constains cluster assignement for each row of the dataset
getClusters <- function(data, k)
{
  clusters <- sample(k, nrow(data), replace = T)
  prevClusters <- clusters
  clusteringEnded <- FALSE
  
  while (clusteringEnded == FALSE) {
    centroids <- getCentroids(data, clusters)
    
    for (i in 1:nrow(data)) {
      clusters[i] <- getNearestCluster(data, i, centroids)
    }
    
    clusteringEnded <- !(FALSE %in% (clusters == prevClusters))
    prevClusters <- clusters
  }
  clusters
}

#' Get the total variation within all clusters found in the dataset by summing
#' the squared euclidean distance between each pair of values that belong to the same cluster
#' @param data data.frame The dataset
#' @param clusters vector The current clusters assigned to each row of data
#' @return double The amount of within-cluster variation
getClustersVariation <- function(data, clusters) {
  variation <- 0
  k <- max(clusters)
  ncols <- ncol(data)
  
  for (cluster in 1:k) {
    clusterSubset <- data[clusters == cluster,]
    rows <- as.numeric(rownames(clusterSubset))
    clusterVariation <- 0
    
    for (i in rows) {
      for (i2 in rows) {
        for (j in 1:ncols) {
          clusterVariation <- variation + (data[i,j] - data[i2,j]) ^ 2
        }
      }
    }
    variation <- variation + (clusterVariation / nrow(clusterSubset))
  }
  variation
}

#' Find k clusters in the dataset by performing K-means multiple times
#' The best clustering is returned, that is the clustering that provide
#' the lowest within-cluster variation
#' @param data data.frame The dataset
#' @param k the number of clusters
#' @param k the number of clustering executions to perform
#' @return vector that constains cluster assignement for each row of the dataset
kmeans <- function(data, k, nbExecutions = 8)
{
  variations <- rep(NA, nbExecutions)
  clustersTries <- vector("list", nbExecutions)
  
  for (i in 1:nbExecutions) {
    clustersTries[[i]] <- getClusters(data, k)
    variations[i] <- getClustersVariation(data, clustersTries[[i]])
  }
  
  clustersTries[[which.min(variations)]]
}