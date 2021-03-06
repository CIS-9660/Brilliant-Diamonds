# <<<<<<< HEAD
# Fall 2021 CIS 9660 - Data Mining for Business Analytics
# Professor Chaoqun Deng
# Final Project Report
# Team 5: Shourya Dharmale, Seoungha (Vanessa) Choi, Hyeongwan (Henry) Gwak, Vivian Kwong, Luke Liao, Xin (Stella) Liu, Mary Ellen Pecora

library(dplyr)
library(data.table)
library(tidyr)
library(car)       # for VIF() function
library(stargazer) # for figures # install.packages("stargazer")
library(tree)
library(randomForest)
library(gbm)


# Read file 
b_diamond=read.csv("BDiamond.csv", header=T,stringsAsFactors = TRUE)
str(b_diamond)
dim(b_diamond)
names(b_diamond)
summary(b_diamond)

# Remove 3 unnecessary columns 
b_diamond = select(b_diamond,-url,-id,-date_fetched) 

# Attach b_diamond
attach(b_diamond)

# Table 1 Summary Results:
# Quantitative variable (Carat)
stargazer(b_diamond[2:3],title="Table 1: Summary Results", 
          out="QuantSummaryTable1.html", digits=2)

# Qualitative bar graphs 
par(mfrow=c(2,3))
plot_cut = barplot(table(cut),main="Cut")
plot_color = barplot(table(color),main="Color")
plot_clarity = barplot(table(clarity),main="Clarity")
plot_shape = barplot(table(shape),main="Shape")
plot_report = barplot(table(report),main="Grade Report Lab")
plot_type = barplot(table(type),main="Lab or Natural")

# 4 Analysis Methods Considered (1.Multiple Linear Regression, 2.Tree-based Methods, 3.Random Forests, 4.Bagging):   
# 1. MULTIPLE LINEAR REGRESSION ---------------------------------------------------------------------------------------------------------
######## Model 1 #############
# Model 1 includes all the variables in b_diamond
# Reference: A2-(1)&(2)
lm.fit=lm(price~.,b_diamond)
summary(lm.fit) 
# RSE: 6946 on 119275 (=0.05823) Adjusted R-squared:  0.4192

# Divide dataset into train and test dataset using the validation set approach and calculate the test error (MSE)
# Reference: Lab3-page 4
set.seed(1)                                             # to get consistent results
train=sample(nrow(b_diamond), nrow(b_diamond)/2)        # validation set approach divides dataset into two halves
lm.fit.train=lm(price~.,b_diamond,subset=train)         # subset=train
mean((price-predict(lm.fit.train,b_diamond))[-train]^2) # MSE = 53,060,285 

######## Model 2 #############
# Model 2 includes all the variables in b_diamond except the "Cut". (We performed the Backwards selection.)
# Choosing a p-value of 10%, remove cut because when we compared to other coefficients, it did not seem as significant.
lm.fit1=update(lm.fit,~.-cut)
summary(lm.fit1) 
# RSE: 6949 on 119279 (=0.05825) Adjusted R-squared:  0.4187
# When we compared the RSE & R-squared with and without Cut, the difference was very minimal, 
# thereby proving our initial conclusion for cut. (Drop Model 2 and keep Model 1) 

######## Model 3 #############
# Model 3 includes all the variables in b_diamond except the "Report" and "Type" which have the col-linearity
# We loaded the library(car) to use the vif() function 
# Reference: Day 7 Multi-linear Regression Slide 31, 43-46, Day 8 pdf (Lab2) slide 25, Lab 2 R file - 3.6.3 Multiple Linear Regression
# VIF stands for variance inflation factor. 
# It measures how much the variance of any one of the coefficients is inflated due to multicollinearity in the overall model.
# As a rule of thumb, a vif score over 5 is a problem. 
# A score over 10 should be remedied and you should consider dropping the problematic variable from the regression model or creating an index of all the closely related variables.

# Table 3: Model 1's VIF results
vif(lm.fit)          
stargazer(vif(lm.fit), title="Table 3: VIF Results", out="VIFTable.html")
#              GVIF Df GVIF^(1/(2*Df))
# shape    1.456078  9        1.021094
# carat    1.425661  1        1.194011
# cut      1.363924  4        1.039558
# color    1.220264  6        1.016727
# clarity  1.237186  7        1.015319
# report  23.952182  3        1.697817 -- 23.95 > 10 --> 'report' should be excluded from lm.fit
# type    21.574678  1        4.644855 -- 21.57 > 10 --> 'type' should be excluded from lm.fit

# Table 4: Model 3's VIF results
lm.fit2=update(lm.fit,~.-report -type) 
vif(lm.fit2)
stargazer(vif(lm.fit2), title="Table 4: VIF Results After Removing Collinear Variables", out="VIFTableAfterRemoving.html")
# GVIF Df GVIF^(1/(2*Df))
# shape   1.390076  9        1.018466
# carat   1.230484  1        1.109272
# cut     1.280525  4        1.031391
# color   1.169947  6        1.013166
# clarity 1.096670  7        1.006613

# Table 2: Multiple Linear Regression Results (Comparison of Model 1, Model 2, and Model 3)
stargazer(lm.fit,lm.fit1,lm.fit2, title="Table 2: Multiple Linear Regression Results", out="LinearRegressionTable.html")

# RSE and R-squared: Model 1 VS Model 3
# Although the collinearity problem is resolved, when we compared the RSE and R-squared of model 1 and model 3, 
# model 3 has higher RSE and lower R-squared, so we decided to drop model 3 and keep model 1. 
summary(lm.fit)  # RSE: 6946 on 119275 (=0.05823) Adjusted R-squared:  0.4192
summary(lm.fit2) # RSE: 7243 on 119279 (=0.0607) Adjusted R-squared:  0.3685

# 2. TREE-BASED METHODS (REGRESSION TREE) ---------------------------------------------------------------------------------------------------------

# tree method with validation set approach
# Use set.seed(1) and train=sample() function to create a training set containing half of the observations are selected as the training dataset while half of observations are treated as the test dataset.
set.seed(1)
train = sample(1:nrow(b_diamond), nrow(b_diamond)/2)
# Use the tree() function to fit a regression tree.
tree.diamond=tree(price~shape+carat+cut+color+clarity+report+type,b_diamond,subset=train)
# Use the summary () function to produce summary statistics about the tree.
summary(tree.diamond)
# Use the plot() function to display the tree structure,and then use the text() function to display the node labels
par(mfrow=c(1,1))
plot(tree.diamond)
text(tree.diamond,pretty=0)
# variables actually used in tree construction are "carat"   "report"  "clarity"

# use cross validation to check if the tree needs to be pruned
# Use the cv,tree() function to the training set in order to determine the optimal tree size.
cv.diamond=cv.tree(tree.diamond)
cv.diamond
plot(cv.diamond$size,cv.diamond$dev,type='b')
# the best tree size is 10, so we don't need to prune the tree

# use unpruned tree to make prediction on the test data
yhat=predict(tree.diamond,newdata=b_diamond[-train,])

# true value of DV on the test data
diamond.test=b_diamond[-train,"price"]
plot(yhat,diamond.test)
abline(0,1)
#MSE (Mean of Squared Errors)
mean((yhat-diamond.test)^2)

# 4. BAGGING ----------------------------------------------------------------------------------------------------------------------------

#set seed
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

#significance of each variable 
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


#3. RANDOM FOREST ---------------------------------------------------------------------------------------------------------

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
