<<<<<<< HEAD
source("DecisionTree.R")
library(parallel)
library(foreach)
library(doParallel)

#Trains a random forest algorithm on a dataset X with labels Y
#nbTrees represents the number of decision trees
#Theta is the maximum entropy of a leaf in the trees
#r is the number of randomly selected variables in each node
trainRandomForest <- function(X,Y, nbTrees, theta, r){
  '
  require(parallel)
  require(foreach)
  require(doParallel)
  vars2export <- c("decisionTree", "entropy","divideDataset", "orderVectorByOther", "separations", "separate",
                   "is.qualitative", "attributeDivision", "decisionTree", "decisionTree.predict", "decisionNode")
  registerDoParallel(detectCores()) 
  '
  data = cbind(X,Y)
  "
  trainedTrees = foreach(i =  1:nbTrees, .export = vars2export)%dopar%{
    d = data[sample(nrow(data),replace = TRUE),]
    localX = d[, -ncol(d)]
    localY = d[, ncol(d)] 
    decisionTree(localX,localY,theta, r)
  }
  "
  trainedTrees = list()
  for (i in  1:nbTrees){
    d = data[sample(nrow(data),replace = TRUE),]
    localX = d[, -ncol(d)]
    localY = d[, ncol(d)] 
    trainedTrees[[i]] = decisionTree(localX,localY,theta, r)
  }
  return (trainedTrees)
}

#Predicts the class of x by the random forest model "trees"
predictRandomForest = function(x, Trees) {
  '
  require(parallel)
  require(foreach)
  require(doParallel)
  vars2export <- c("decisionTree.predict")
  registerDoParallel(detectCores())
  predictions = unlist(foreach(tree = trees, .export=vars2export) %dopar% decisionTree.predict(tree, x))
  '
  predictions = list()
  for (i in 1:length(Trees)) {
    predictions[[i]] = decisionTree.predict(Trees[[i]], x)
  }
  predictions = as.factor(unlist(predictions))
  return (levels(predictions)[which.max(tabulate(predictions))])
}