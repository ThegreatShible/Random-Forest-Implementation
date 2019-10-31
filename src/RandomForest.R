source("src/DecisioNTree.R")
library(parallel)
library(foreach)
library(doParallel)

trainRandomForest <- function(X,Y, nbTrees, theta, r){
  require(parallel)
  require(foreach)
  require(doParallel)
  vars2export <- c("decisionTree", "entropy","divideDataset", "orderVectorByOther", "separations", "separate",
                   "is.qualitative", "attributeDivision", "decisionTree", "decisionTree.predict", "decisionNode")
  registerDoParallel(detectCores()) 
  data = cbind(X,Y)
  trainedTrees = foreach(i =  1:nbTrees, .export = vars2export)%dopar%{
    d = data[sample(nrow(data),replace = TRUE),]
    localX = d[, -ncol(d)]
    localY = d[, ncol(d)] 
    decisionTree(localX,localY,theta, r)
  }
  return (trainedTrees)
}


predictRandomForest = function(x, Trees) {
  require(parallel)
  require(foreach)
  require(doParallel)
  vars2export <- c("decisionTree.predict")
  registerDoParallel(detectCores())
  predictions = unlist(foreach(tree = trees, .export=vars2export) %dopar% decisionTree.predict(tree, x))
  return (which.max(tabulate(factor(predictions))))
}
