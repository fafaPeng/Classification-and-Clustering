library(ISLR)
library(class)#KNN
data(Caravan)
set.seed(1)

dim(Caravan)
attach(Caravan)
summary(Purchase)
#0.0598 people purchased caravan insurance 

#standardize the data so that all variables are given a mean of zero and a standard deviation of one
#then all variables will be on a  scale
#the column 86 is the qualitative purchase variable 
standardized.X=scale(Caravan[,-86])
var(Caravan[ ,1])
var(Caravan[ ,2])
var(standardized.X[,1])
var(standardized.X[,2])

#split6 the observations into a test set, containing the first 1000 observations, 
#and the training set containing the remaining observations
test=1:1000
train.X= standardized.X[-test ,]
test.X= standardized.X[test ,]
train.Y=Purchase [-test]
test.Y=Purchase [test]

#fit KNN model on the training data using k=1
knn.pred=knn(train.X,test.X,train.Y,k=2)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred ,test.Y)

knn.pred=knn(train.X,test.X,train.Y,k=3)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred ,test.Y)

knn.pred=knn(train.X,test.X,train.Y,k=5)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred ,test.Y)