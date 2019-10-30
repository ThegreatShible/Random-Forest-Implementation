source("src/DecisioNTree.R")
library(parallel)
library(foreach)


#TODO : Ajouter les imports dans le foreach 
RandomForest <- setRefClass("RandomForest",
   fields = list(nbTrees = "integer", theta = "numeric"),
   methods = list(
     train = function(X,Y) {
       trainedTrees = foreach(i = 1:nbTrees) %dopar% {
         decisionTree(X,Y,theta)
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

