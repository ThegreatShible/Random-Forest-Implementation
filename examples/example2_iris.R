data("iris")
source("DecisionTree.R")
source("RandomForest.R")
iris

shuffled = iris[sample(nrow(iris)),]
trainProportion = (60/100) * nrow(iris)
train = shuffled[1:trainProportion,]
test = shuffled[(trainProportion+1):nrow(shuffled),]

Xtrain = train[,-ncol(train)]
#Xtrain["Petal.Length.Int"] = as.factor(floor(Xtrain[,"Petal.Length"]))

Xtest = test[,-ncol(test)]
#Xtest["Petal.Length.Int"] = as.factor(floor(Xtest[,"Petal.Length"]))

Ytrain = train[,ncol(train)]

tree = decisionTree(Xtrain, Ytrain, 0, 2)

rateTree=0
for (i in (1:nrow(Xtest))) {
  line = Xtest[i,]
  prediction = decisionTree.predict(tree, line)
  answer = test[i,][,ncol(test)]
  if (answer != prediction) {
    rateTree = rateTree + 1
  }
}
rateTree = rateTree / nrow(test)

forest = trainRandomForest(Xtrain, Ytrain, 50, 0, 2)
rateForest=0
for (i in (1:nrow(Xtest))) {
  line = Xtest[i,]
  prediction = predictRandomForest(line, forest)
  answer = test[i,][,ncol(test)]
  if (answer != prediction) {
    rateForest = rateForest + 1
  }
}
rateForest = rateForest / nrow(test)

print(rateTree)
print(rateForest)

plot(iris[,"Petal.Length"], iris[,"Petal.Width"], col=c("red", "green", "blue")[iris[,"Species"]])
