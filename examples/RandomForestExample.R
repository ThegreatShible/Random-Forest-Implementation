source("src/RandomForest.R")
data("iris")
iris

shuffled = iris[sample(nrow(iris)),]
trainProportion = (60/100) * nrow(iris)
train = shuffled[1:trainProportion,]
extraTrain <- sample(1:300,  dim(train)[1], TRUE)

test = shuffled[(trainProportion+1):nrow(shuffled),]
extraTest <- sample(1:300,  dim(test)[1], TRUE)
Xtrain = train[,-ncol(train)]
Xtrain["extra"] <- factor(extraTrain)
#Xtrain = Xtrain["extra"]
Ytrain = train[,ncol(train)]
Xtest = test[,-ncol(test)]
Xtest["extra"] <- factor(extraTest)
Ytest <- test[,ncol(test)]
model <- trainRandomForest(Xtrain, Ytrain, 500, 0.1, 2)
acc = 0
nrow = nrow(Xtest)
for(i in 1:nrow){
  Xt <- Xtest[i,]
  Yt <- Ytest[i]
  pred <- predictRandomForest(Xt,model)
  print(pred)
  if(pred == Yt) acc = acc+1
}
print(acc/nrow)