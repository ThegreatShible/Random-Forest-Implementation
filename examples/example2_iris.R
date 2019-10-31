data("iris")
source("decisionTree.R")
iris
tree = decisionTree(iris[,-ncol(iris)], iris[,ncol(iris)], 0.3)
printTree(tree)
rate=0
for (i in (1:nrow(iris))) {
  line = iris[i,]
  prediction = decisionTree.predict(tree, line[,-ncol(iris)])
  answer = line[,ncol(iris)]
  if (answer != prediction) {
    print(line)
    print(p)
    print("-----")
    rate = rate + 1
  }
}
rate = rate / nrow(iris)
print(rate)
