data("iris")
source("decisionTree.R")
iris
tree = decisionTree(iris[,-ncol(iris)], iris[,ncol(iris)], 0.3)
printTree(tree)
decisionTree.predict(tree, iris[1,-ncol(iris)])
