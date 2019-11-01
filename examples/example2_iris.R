data("iris")
source("decisionTree.R")
iris

shuffled = iris[sample(nrow(iris)),]
trainProportion = (60/100) * nrow(iris)
train = shuffled[1:trainProportion,]
#extraTrain <- sample(1:300,  dim(train)[1], TRUE)

test = shuffled[(trainProportion+1):nrow(shuffled),]
#extraTest <- sample(1:300,  dim(test)[1], TRUE)

Xtrain = train[,-ncol(train)]
Xtrain["Petal.Length.Int"] = as.factor(floor(Xtrain[,"Petal.Length"]))

Xtest = test[,-ncol(test)]
Xtest["Petal.Length.Int"] = as.factor(floor(Xtest[,"Petal.Length"]))
#Xtrain["extra"] <- factor(extraTrain)
#Xtrain = Xtrain["extra"]
Ytrain = train[,ncol(train)]
tree = decisionTree(Xtrain, Ytrain, 0.4, 2)


printTree(tree)
rate=0
for (i in (1:nrow(Xtest))) {
  line = Xtest[i,]
  prediction = decisionTree.predict(tree, line)
  answer = test[i,][,ncol(test)]
  if (answer != prediction) {
    print(line)
    print(prediction)
    print("-----")
    rate = rate + 1
  }
}
rate = rate / nrow(test)
print(rate)

# Attributes 3 and 4 seems to be sufficient to predict iris type
# Let's plot them

plot(iris[,"Petal.Length"], iris[,"Petal.Width"], col=c("red", "green", "blue")[iris[,"Species"]])
