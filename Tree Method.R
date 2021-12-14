# The tree library is used to construct Regression Decision Tree
library(tree)
# header=T tells R that the first line of the file contains the variable names; na.strings tell R
# that any time it sees a particular character or set of characters (such as a question mark), it should be treated as a missing element of the data matrix.
b_diamond=read.csv('BDiamond.csv',header=T,na.strings="?")
#na.omit: removes the rows contain missing observations
b_diamond=na.omit(b_diamond)
#nrow: find out number of rows in data.
nrow(b_diamond)
names(b_diamond)
attach(b_diamond)

#converts quantitative variables into qualitative variables
b_diamond$shape=as.factor(b_diamond$shape)
b_diamond$cut=as.factor(cut)
b_diamond$color=as.factor(color)
b_diamond$clarity=as.factor(clarity)
b_diamond$report=as.factor(report)
b_diamond$type=as.factor(type)

# tree method with validation set approach
set.seed(1)
train = sample(1:nrow(b_diamond), nrow(b_diamond)/2)
tree.diamond=tree(price~shape+carat+cut+color+clarity+report+type,b_diamond,subset=train)
summary(tree.diamond)
plot(tree.diamond)
text(tree.diamond,pretty=0)
# variables actually used in tree construction are "carat"   "report"  "clarity"

# use cross validation to check if the tree needs to be pruned
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

