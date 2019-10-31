data("iris")
source("decisionTree.R")
iris

shuffled = iris[sample(nrow(iris)),]
trainProportion = (80/100) * nrow(iris)
train = shuffled[1:trainProportion,]
test = shuffled[(trainProportion+1):nrow(shuffled),]

tree = decisionTree(train[,-ncol(train)], train[,ncol(train)], 0.3)
printTree(tree)
rate=0
for (i in (1:nrow(train))) {
  line = train[i,]
  prediction = decisionTree.predict(tree, line[,-ncol(test)])
  answer = line[,ncol(test)]
  if (answer != prediction) {
    print(line)
    print(p)
    print("-----")
    rate = rate + 1
  }
}
rate = rate / nrow(test)
print(rate)
