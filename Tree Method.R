library(tree)
Diamond=read.csv('BDiamond.csv',header=T,na.strings="?")
Diamond=na.omit(Diamond)
nrow(Diamond)
names(Diamond)
attach(Diamond)


Diamond$shape=as.factor(Diamond$shape)
Diamond$cut=as.factor(cut)
Diamond$color=as.factor(color)
Diamond$clarity=as.factor(clarity)
Diamond$report=as.factor(report)
Diamond$type=as.factor(type)

# tree method with validation set approach
set.seed(1)
train = sample(1:nrow(Diamond), nrow(Diamond)/2)
# tree including all the variables
tree.diamond=tree(price~.,Diamond,subset=train)
summary(tree.diamond)
plot(tree.diamond)
text(tree.diamond,pretty=0)

# tree including selected variables
set.seed(1)
tree.diamond1=tree(price~shape+carat+cut+color+clarity+report+type,Diamond,subset=train)
summary(tree.diamond1)
plot(tree.diamond1)
text(tree.diamond1,pretty=0)
#Both trees produce the same result, variables actually used in tree construction are
# "carat"   "report"  "clarity" "cut" 


cv.diamond=cv.tree(tree.diamond)
cv.diamond
plot(cv.diamond$size,cv.diamond$dev,type='b')

prune.diamond=prune.tree(tree.diamond,best=10)
plot(prune.diamond)
text(prune.diamond,pretty=0)
summary(prune.diamond)


yhat=predict(tree.diamond,newdata=Diamond[-train,])



# true value of DV on the test data
diamond.test=Diamond[-train,"price"]
plot(yhat,diamond.test)
abline(0,1)
#MSE (Mean of Squared Errors)
mean((yhat-diamond.test)^2)



