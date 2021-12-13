#when we compared the R squared with and without cut, the difference was very minimal, thereby proving our initial conclusion for cut
library(dplyr)
library(data.table)
library(tidyr)
library(randomForest)
library(gbm)
b_diamond=read.csv("BDiamond.csv", header=T)
#str(b_diamond) do we need this??
dim(b_diamond)
names(b_diamond)
#remove all arbitrary columns to ensure cleaner analysis, note that library dplyr is required to use select() function
b_diamond = select(b_diamond, -url,-id,-date_fetched)
summary(b_diamond)
attach(b_diamond)
lm.fit=lm(price~shape+carat+cut+color+clarity+report+type)
summary(lm.fit)
#predict(lm.fit,b_diamond, interval="confidence")
#choosing a p-value of 10%, remove cut because when we compared to other coefficients, it did not seem as significant.
lm.fit1=update(lm.fit,~.-cut)
summary(lm.fit1)

#Compare relationship between 4C's and Diamond type (lab/natural) to see how it affects y variable price
b_diamond %>%
  group_by(cut) %>%
  summarise(n=n(), 
            mean= mean(price), 
            median=median(price), 
            Q1= quantile(price,0.25),
            Q3= quantile(price,0.75))
b_diamond %>%
  group_by(clarity) %>%
  summarise(n=n(), 
            mean= mean(price), 
            median=median(price), 
            Q1= quantile(price,0.25),
            Q3= quantile(price,0.75))
b_diamond %>%
  group_by(carat) %>%
  summarise(n=n(), 
            mean= mean(price), 
            median=median(price), 
            Q1= quantile(price,0.25),
            Q3= quantile(price,0.75))
b_diamond %>%
  group_by(color) %>%
  summarise(n=n(), 
            mean= mean(price), 
            median=median(price), 
            Q1= quantile(price,0.25),
            Q3= quantile(price,0.75))
b_diamond %>%
  group_by(type) %>%
  summarise(n=n(), 
            mean= mean(price), 
            median=median(price), 
            Q1= quantile(price,0.25),
            Q3= quantile(price,0.75))

#changed qualitative variables to quantitative using as.factor method 
b_diamond[,1]=as.factor(factor(b_diamond[,1]))
b_diamond[,4]=as.factor(factor(b_diamond[,4]))
b_diamond[,5]=as.factor(factor(b_diamond[,5]))
b_diamond[,6]=as.factor(factor(b_diamond[,6]))
b_diamond[,7]=as.factor(factor(b_diamond[,7]))
b_diamond[,8]=as.factor(factor(b_diamond[,8]))
pairs(~price+color+clarity+cut+type+report+shape+carat,b_diamond)
pairs(b_diamond)
#randomforest &  bagging, start by importing the libraries random forest and gbm
set.seed(1)
# set the training dataset
train = sample(1:nrow(b_diamond), nrow(b_diamond)/2)
# set the DV ("price") on the test data
diamond.test=b_diamond[-train,"price"]
set.seed(1)
# The argument mtry=7 indicates that all 7 predictors should be considered for each split of the tree.In other words, that bagging should be done
bag.diamond=randomForest(price~.,data=b_diamond,subset=train,mtry=7,importance=TRUE)
bag.diamond
#the MSE is rather large due to the dataset, as 9,506,905. Variance is 87.54
# How well does this bagged model perform on the test set by predicting the MSE on the test data
yhat.bag = predict(bag.diamond,newdata=b_diamond[-train,])
plot(yhat.bag, diamond.test)
abline(0,1)
mean((yhat.bag-diamond.test)^2)
#importance of bag.diamond
importance(bag.diamond)
# plot the importance of each variable
varImpPlot(bag.diamond)
# The test set MSE associated with the bagged regression tree is 17,538,424 smaller han the one 35M using an optimally-pruned single tree.
# Bagging improved the prediction accuracy than a single tree
# We changed the number of trees grown by randomForest() using the ntree argument
# E.g. change the number of trees to 100 (by multiples of 25)
bag.diamond100=randomForest(price~.,data=b_diamond,subset=train, mtry=7,ntree=100,importance=TRUE)
yhat.bag100 = predict(bag.diamond100,newdata=b_diamond[-train,])
mean((yhat.bag100-diamond.test)^2)

# The MSE with a smaller number of trees is larger at 23,975,491. Therefore, we will keep the larger number of trees
# Random Forest
# By default, 1) RandomForest () uses p/3 variables when building a random forest of regression trees, and 2) use square root (p) variables when building a random forest of classification trees. Here we
# use mtry = 7/3. As stated before, since price is the DV, the number of predictors should be 7.
set.seed(1)
rf.diamond=randomForest(price~.,data=b_diamond,subset=train,mtry=7/3,importance=TRUE)
yhat.rf = predict(rf.diamond,newdata=b_diamond[-train,])
mean((yhat.rf-diamond.test)^2)
#in random forest, the MSE (23,975,491) is much higher than bagging. Bagging appears to be the best method, so long as a large number of trees is selected.
#here, we weigh the importance of each variable 
importance(rf.diamond)
# plot the importance of each variable
varImpPlot(rf.diamond)
#The carat predictor by far had the highest %IncMSE at 71.41, with type following at 17.45. These are considered to be the most important x variables in our dataset.
# We ruled out the method of boosting for this dataset. Unfortunately, most of our variables are qualitative and gaussian will only work with one quantitative variable, carat. Therefore, we cannot compare the boosting method to others since not all variables can be compared.
