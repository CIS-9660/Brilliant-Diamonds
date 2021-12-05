b_diamond=read.csv("BDiamond.csv", header=T)
str(b_diamond)
dim(b_diamond)
names(b_diamond)
#remove all arbitrary columns to ensure cleaner analysis
b_diamond = select(b_diamond, -url,-id,-date_fetched)
summary(b_diamond)
attach(b_diamond)
lm.fit=lm(price~shape+carat+cut+color+clarity+report+type)
summary(lm.fit)
#predict(lm.fit,b_diamond, interval="confidence")
#choosing a p-value of 10%, remove cut because when we compared to other coefficients, it did not seem as significant.
lm.fit1=update(lm.fit,~.-cut)
summary(lm.fit1)
#when we compared the R squared with and without cut, the difference was very minimal, thereby proving our initial conclusion for cut
library(dplyr)
library(data.table)
library(tidyr)
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
#plot(carat, price, col="red")
#b_diamond$color=as.factor(color)
#x_num <- as.numeric(unlist(b_diamond))
install.packages("GGally")
install.packages("ggplot2")
library(ggplot2)
library(GGally)
ggpairs(b_diamond)

#changed qualitative variables to quantitative using as.factor method 
b_diamond[,1]=as.factor(factor(b_diamond[,1]))
b_diamond[,4]=as.factor(factor(b_diamond[,4]))
b_diamond[,5]=as.factor(factor(b_diamond[,5]))
b_diamond[,6]=as.factor(factor(b_diamond[,6]))
b_diamond[,7]=as.factor(factor(b_diamond[,7]))
b_diamond[,8]=as.factor(factor(b_diamond[,8]))
pairs(~price+color+clarity+cut+type+report+shape+carat,b_diamond)
pairs(b_diamond)
