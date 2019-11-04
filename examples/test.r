
# replace Y par num | train par train

## transformation data ####
library(randomForest)
require(caTools)

data <- read.csv(
  "examples/processed.cleveland.data",
  header=FALSE
)
dim(data)
names(data) <- c("age", "sex", "cp", "trestbps", "choi", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thai", "num")
head(data)
data$num[data$num > 1] <- 1
summary(data)
sapply(data, class)


data <- transform(
  data,
  age=as.integer(age),
  sex=as.factor(sex),
  cp=as.factor(cp),
  trestbps=as.integer(trestbps),
  choi=as.integer(choi),
  fbs=as.factor(fbs),
  restecg=as.factor(restecg),
  thalach=as.integer(thalach),
  exang=as.factor(exang),
  oldpeak=as.numeric(oldpeak),
  slope=as.factor(slope),
  ca=as.factor(ca),
  thai=as.factor(thai),
  num=as.factor(num)
)
sapply(data, class)

data[ data == "?"] <- NA
colSums(is.na(data))

data$thai[which(is.na(data$thai))] <- as.factor("3.0")
data <- data[!(data$ca %in% c(NA)),]
colSums(is.na(data))

data$ca <- factor(data$ca)
data$thai <- factor(data$thai)
summary(data)

## train / test ####
sample = sample.split(data$num, SplitRatio = .75)
train = subset(data, sample == TRUE)
dtest  = subset(data, sample == FALSE)
dim(train)
dim(dtest)

## random forest #### 
mod_RF <- randomForest(num~.,data=train)

imp <- importance(mod_RF)
order(imp,decreasing=TRUE)
mod_RF$err.rate[500,1]


prev <- predict(mod_RF,newdata=dtest)
sum(prev!=dtest$num)/nrow(dtest)

## random forest from scratch
forest = trainRandomForest(train[,-14],train[,14], 10, 0, 2)
rate = 0
for (i in 1:nrow(dtest)) {
  t = dtest[i,]
  pre = predictRandomForest(t[,-14], forest)
  if (pre != t[,14]) rate = rate + 1
}
rate = rate / nrow(dtest)
rate

## arbre de décision ####
library(rpart)
train <- train
train$num <- as.factor(train$num)
arbre <- rpart(num~.,data=train)
par(mfrow=c(1,2))
plot(arbre)
text(arbre,pretty=0)
plotcp(arbre)

arbre1 <- rpart(num~.,data=train,minsplit=5,cp=0.002)
prev_arbre <- predict(arbre1,newdata=dtest,type="class")
err_arbre <- sum(prev_arbre!=dtest$num)/nrow(dtest)
err_arbre


#Adaboost
library(fastAdaboost)
boost_err = c()
base = 19
for (nIter in 20:100){
  print(nIter)
  model <- adaboost(num~.,data=train, nIter = nIter)
  predictions <- unlist(apply(predict(model, dtest[,-14])$prob, 1, function(x) which.max(x) -1))
  boost_err[nIter- base] <- sum(dtest$num != predictions)/nrow(dtest)
}
print(which.min(boost_err))
print(min(boost_err))
# plot(1:1500,mod_RF1$test$err.rate[1:1500,1],type="l",ylim=c(0.03,0.23),
#        ylab="erreur",xlab="nombre d’iterations")
# lines(boucle,err_boost,col="red")
# abline(h=min(err),col="blue")
# abline(h=err_arbre,col="green")

# k ppv ####
library(class)
K <- seq(1,50,by=1)
err <- K
ind <- 0
for (i in K){
  ind <- ind+1
  mod_ppv <- knn(train=train[,-14],test=dtest[,-14],cl=train$num,k=K[ind])
  
  err[ind] <- sum(mod_ppv!=dtest[,14])/nrow(dtest)
}
print(which.min(err))

min(err)
