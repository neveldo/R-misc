#' Attempt of implementation of Decision Tree Regression
#' @author Vincent Brouté

#' split the data into two regions of data in which values are either
#' inferior to data[i,j] or superior or equal to data[i,j]
#' @param data The dataset to split into two parts
#' @param int i The row number of the split value
#' @param string j The column name of the split value
#' @return a list that contains the two splitted parts of the dataset
getRegions <- function(data, i, j)
{
  splitValue <- data[i,j]
  
  list(
    left = subset(data, data[,j] < splitValue), 
    right = subset(data, data[,j] >= splitValue)
  )
}

#' Get the sum of RSSs (Residual Sum Of Squares) for all the regions in the list
#' Assume that the response variable stands in the last column
#' @param list regions list of several part from the original datasets (splitted into regions)
#' @return double sum of RSSs for all regions
getTotalRss <- function(regions)
{
  rss <- 0
  for (i in 1:length(regions)) {
    region <- regions[[i]]
    rss <- rss + sum((region[,ncol(region)] - mean(region[,ncol(region)]))^2)
  }
  rss
}

#' Create and return a new tree node. Each node contain its related dataset region,
#' the predictor name that best split the region into two sub parts by minimizing the total RSS,
#' the split value, the depth of the node in the tree, the left and right sub nodes indexes
#' @param data.frame data the related dataset region
#' @param string splitPredictor the predictor name that best split the region into two sub parts
#' @param double splitValue
#' @param int depth the depth of the node in the tree
#'
createNode <- function(data, splitPredictor, splitValue, depth)
{
  list(
    data = data,
    splitPredictor = splitPredictor,
    splitValue = splitValue,
    depth = depth,
    leftChildNode = NA,
    rightChildNode = NA
  )
}

#' Return TRUE if the node is a terminal node (leaf)
#' @param list node
#' @return boolean TRUE if the node is a terminal node (leaf)
isTerminalNode <- function(node)
{
  is.na(node$leftChildNode) && is.na(node$rightChildNode)
}

# Split the data by evaluating RSS on split on all predictors and all values

#' Grow and return a decision tree based in the data.frame data
#' The tree is structured into a list of nodes. Each node is also a list and contains a reference to its left and right child node.
#' @param data.frame All column except the last one are assumed to be predictors variables. The
#' last one is assumed to be the quantitative response variable
#' @param int maxDepth The max depth until stopping to grow the tree
#' @param int minNodeRecords The minimum number of records allowed in a node until stopping to grow the tree
#' 
#' @todo Implement tree pruning
#' @todo Extend decision tree for categorical response (with GINI index ?)
#' @todo Allow categorical predictor variables
decisionTree <- function(data, maxDepth = 10, minNodeRecords = 3)
{
  maxDepthReached <- F
  minNodeRecordsReached <- F
  
  # Init the root node with the full dataset
  tree <- list(createNode(data, NA, NA, 1))
  
  while (!maxDepthReached && !minNodeRecordsReached) {
    
    bestNodeToSplit <- NA
    bestRss <- NA
    bestSplitPredictor <- NA
    bestRegions <- list()
    bestSplitValue <- NA
    
    # Try grow two new branches by checking the data from each terminal nodes
    for (nodeIndex in 1:length(tree)) {
      if (!isTerminalNode(tree[[nodeIndex]])) next
      
      # Init regions list that contain the data from all terminal nodes except the current one
      # In order to further compute the total RSS for each split attempt
      regions <- list()
      if (length(tree) > 1) {
        for (i in 1:length(tree)) {
          if (i == nodeIndex || !isTerminalNode(tree[[i]])) next
          regions[[length(regions) + 1]] <- tree[[i]]$data
        }
      }
      
      # Try to split the data into two parts for each value of the data from the current terminal node checked
      for (j in colnames(data)[-length(colnames(data))]) {
        for (i in rownames(tree[[nodeIndex]]$data)) {
          # Get the two candidate regions of data to check
          regionsToEvaluate <- getRegions(tree[[nodeIndex]]$data, i, j)
          
          if (nrow(regionsToEvaluate$left) == 0 || nrow(regionsToEvaluate$right)  == 0) next
          
          # Evalute the total RSS with this split candidate
          rss <- getTotalRss(c(regionsToEvaluate, regions))
          
          # Store the candidate if better than the previous stored one ...
          if (is.na(bestRss) || rss < bestRss) {
            bestRss <- rss
            bestSplitPredictor <- j
            bestSplitValue <- tree[[nodeIndex]]$data[i,j]
            bestRegions <- regionsToEvaluate
            bestNodeToSplit <- nodeIndex
          }
        }
      }
    }
    
    # Create the two new child nodes and set all the needed data
    newDepth <- tree[[bestNodeToSplit]]$depth + 1
    leftNodeIndex <- length(tree) + 1
    rightNodeIndex <- length(tree) + 2
    
    tree[[bestNodeToSplit]]$splitPredictor <- bestSplitPredictor
    tree[[bestNodeToSplit]]$splitValue <- bestSplitValue
    tree[[bestNodeToSplit]]$leftChildNode <- leftNodeIndex
    tree[[bestNodeToSplit]]$rightChildNode <- rightNodeIndex
    
    tree[[leftNodeIndex]] <- createNode(bestRegions$left, NA, NA, newDepth)
    tree[[rightNodeIndex]] <- createNode(bestRegions$right, NA, NA, newDepth)
    
    maxDepthReached <- newDepth >= maxDepth
    minNodeRecordsReached <- nrow(bestRegions$left) <= minNodeRecords || nrow(bestRegions$right) <= minNodeRecords
  }
  
  # Clean all the data stored into each nodes in order to free space, and compute the prediction value
  # Into each terminal node
  for (nodeIndex in 1:length(tree)) {
    if (isTerminalNode(tree[[nodeIndex]])) {
      tree[[nodeIndex]]$prediction <- mean(tree[[nodeIndex]]$data[,'y'])
    }
  tree[[nodeIndex]]$data <- NULL
  }
  
  tree
}

#' Print a text version of the tree
#' @param list tree The tree to print, built by decisionTree()
#' @param int node
#' @param int level
printTree <- function(tree, node = 1, level = 0)
{
  if (!isTerminalNode(tree[[node]])) {
      
    cat(rep(".", 2*level), sep = "")
    cat(tree[[node]]$splitPredictor, " < ", tree[[node]]$splitValue)
    
    writeLines("")
    
    if (!is.na(tree[[node]]$leftChildNode)) {
      printTree(tree, tree[[node]]$leftChildNode, level + 1)
    }
    
    cat(rep(".", 2*level), sep = "")
    cat(tree[[node]]$splitPredictor, " >= ", tree[[node]]$splitValue)
    
    writeLines("")
    
    if (!is.na(tree[[node]]$rightChildNode)) {
      printTree(tree, tree[[node]]$rightChildNode, level + 1)
    }
  } else {
    cat(rep(".", 2*level), sep = "")
    cat("prediction : ", tree[[node]]$prediction)
    writeLines("")
  }
}

#' Predict a response for the tree and a data.frame of observations
#' @param list tree to use for prediction, built with decisionTree()
#' @param observations The observations to use for prediction
predictFromTree <- function(tree, observations)
{
  predictions <- data.frame(y = rep(NA, nrow(observations)))
  i <- 1
  
  for (row in rownames(observations)) {
  
    observation <- observations[row,, drop = F]
    
    prediction <- NA
    nodeToCheck <- 1
    
    while(is.na(prediction)) {
      if (isTerminalNode(tree[[nodeToCheck]])) {
        prediction <- tree[[nodeToCheck]]$prediction
      } else {
        if (observation[tree[[nodeToCheck]]$splitPredictor] < tree[[nodeToCheck]]$splitValue) {
          nodeToCheck <- tree[[nodeToCheck]]$leftChildNode
        } else {
          nodeToCheck <- tree[[nodeToCheck]]$rightChildNode
        }
      }
    }
    
    predictions[i,1] <- prediction
    i <- i + 1
  }
  predictions
}