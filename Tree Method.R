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
tree.diamond=tree(price~shape+carat+cut+color+clarity+report+type,Diamond,subset=train)
summary(tree.diamond)
plot(tree.diamond)
text(tree.diamond,pretty=0)
#Both trees produce the same result, variables actually used in tree construction are
# "carat"   "report"  "clarity" "cut" 


