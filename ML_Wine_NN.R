#Package
#install.packages("neuralnet")

#Tutorials
#https://rpubs.com/vitorhs/iris?fbclid=IwAR2ReCTNm_CRYoIsNqanKjdWgx3ZVTlSgKzXNd8q9YWYOb0WFX0JeX6ZPGw
#https://hub.packtpub.com/training-and-visualizing-a-neural-network-with-r/?fbclid=IwAR0QyE_CqKlj_RlC4hQn_jC-uXPWeVNgKz5UAlvFj5pjQbyjsJc3Y78Loio
#https://bi4tech.blogspot.com/2015/06/neural-network-with-r-predicting-wine.html?fbclid=IwAR2-Q27j3ZCHgaW_o_GPOBX6fd8oPRHER2pWMCJaa8cQzXlutkIBP-bxnJo


wine = read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"),header = TRUE, sep = ";")

#wine
#summary(wine)

wine$quality = as.factor(wine$quality)
pairs(wine[,-12],col=wine[,12])

dim (wine)
names(wine)
n = nrow(wine)
p = ncol(wine)
  

#Splitting the data in 2 parts in approximately 2n/3 and n/3
Ind.test = c(sample(1:400,n/6),sample(401:800,n/6),sample(801:1200,n/6),sample(1201:1599,n/6))
Learn = wine[Ind.test,]
Test = wine[-Ind.test,]

print(dim(Learn))
print(dim(Test))


#Neural network

Learn$four <- Learn$quality=="4"
Learn$five <- Learn$quality=="5"
Learn$six <- Learn$quality=="6"
Learn$seven <- Learn$quality=="7"
Learn$eight <- Learn$quality=="8"

library(neuralnet)
wine.net <- neuralnet(four+five+six+seven+eight ~ 
                        fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + 
                        free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, 
                      data=Learn, hidden=5)

plot(wine.net, rep="best")
  
wine.net$result.matrix
