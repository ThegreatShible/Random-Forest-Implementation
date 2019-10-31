source("src/DecisioNTree.R")
library(parallel)
library(foreach)


#TODO : Ajouter les imports dans le foreach 
RandomForest <- setRefClass("RandomForest",
   fields = list(nbTrees = "integer", theta = "numeric"),
   methods = list(
     train = function(X,Y) {
       data = cbind(X,Y)
       trainedTrees = foreach(i = 1:nbTrees) %dopar% {
         d = data[sample(nrow(data)),]
         localX = d[, -ncol(d)]
         localY = d[, ncol(d)] 
         decisionTree(localX,localY,theta)
       }
       return (TrainedRandomForest(trees = trainedTrees))
     }
     
     
   ))

TrainedRandomForest <- setRefClass("TrainedRandomForest",
   fields = c("trees"),
   methods = list(
     predict = function(x) {
       predictions = foreach(tree = 1:length(trees), .export="decisionTree.predict") %dopar% decisionTree.predict(tree, x)
       return (which.max(tabulate(predictions)))
     }
   ))

