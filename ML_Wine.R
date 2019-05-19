library (caret)

ERR1 = c()
ERR2 = c()
ERR3 = c()
ERR4 = c()
ERR5 = c()

wine = read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"),header = TRUE, sep = ";")

wine

summary(wine)

wine$quality = as.factor(wine$quality)
pairs(wine[,-12],col=wine[,12])

dim (wine)
names(wine)
n = nrow(wine)
p = ncol(wine)
n
p

#for (x in 1:50) {
  
  #Splitting the data in 2 parts in approximately 2n/3 and n/3
  Ind.test = c(sample(1:400,n/6),sample(401:800,n/6),sample(801:1200,n/6),sample(1201:1599,n/6))
  Learn = wine[Ind.test,]
  Test = wine[-Ind.test,]
  
  print(dim(Learn))
  print(dim(Test))

  #Logistic regression
  
  library(nnet)
  reg = multinom(quality~ .,data=Learn)
  summary(reg)
  prev = predict(reg, newdata=Test)
  table(prev,Test$quality)
  err.logistic = 100* mean(prev != Test$quality)
  ERR1 = c(ERR1, err.logistic)
  
  
  #Cart
  library(rpart)
  cart = rpart(quality ~.,data=wine)
  cart = rpart(quality ~.,data=Learn)
  prev = predict(cart,newdata=Test)
  prev = predict(cart,newdata=Test,type="class")
  table(prev,Test$quality)
  err.cart = 100* mean(prev != Test$quality)
  ERR2 = c(ERR2, err.cart)
  
  
  #Random forest
  
  library(randomForest)
  randf = randomForest(quality ~., data=Learn)
  prev = predict(randf,newdata=Test)
  prev = predict(randf,newdata=Test,type="class")
  table(prev,Test$quality)
  err.rf = 100* mean(prev!= Test$quality)
  ERR3 = c(ERR3, err.rf)
  
  
  #knn
  library(class)
  prev = knn(Learn[,-12],Test[,-12],Learn[,12],k=5)
  table(prev,Test$quality)
  err.knn = 100* mean(prev != Test$quality)
  ERR4 = c(ERR4,err.knn)
  
  
  #svm
  
  library(e1071)
  library(lattice)
  svmmod1 = svm(quality ~., data=wine)
  svmmod1 = svm(quality ~., data=Learn)
  summary (svmmod1)
  prev = predict (svmmod1, newdata=Test)
  table(prev, Test$quality)
  err.svm = 100 * mean(prev != Test$quality)
  ERR5 = c(ERR5, err.svm)
  
#}
ERR1
ERR2
ERR3
ERR4
ERR5
