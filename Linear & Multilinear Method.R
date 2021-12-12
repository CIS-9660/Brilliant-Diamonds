install.packages("GGally")
install.packages("ggplot2")
library(ggplot2)
library(GGally)
ggpairs(b_diamond)

library(dplyr)
library(data.table)
library(tidyr)
library(car)
library(stargazer)

b_diamond=read.csv("BDiamond.csv", header=T,stringsAsFactors = TRUE)
str(b_diamond)
dim(b_diamond)
names(b_diamond)

#remove all arbitrary columns to ensure cleaner analysis
# Choi - ?select - you need 'dplyr' library to use select()
?select()
b_diamond = select(b_diamond,-url,-id,-date_fetched) 
dim(b_diamond)   # 119307      8
names(b_diamond) # "shape"   "price"   "carat"   "cut"     "color"   "clarity" "report"  "type"   
summary(b_diamond)
attach(b_diamond)

#multiple linear regression 
lm.fit=lm(price~.,b_diamond)
summary(lm.fit) # Choi - RSE: 6946 on 119275 (=0.05823) Adjusted R-squared:  0.4192
# Choi Reference: A2 - 1) & 2)
# Choi - In the paper, we should explain the relationships btw each predictor and dv, based on each variable's p-value. - significant or not, negative or positive. 

# Choi Reference: A2 - 2) - d
# predict() function to obtain prediction interval or confidence intervals 
# predict(lm.fit,interval="confidence")

#choosing a p-value of 10%, remove cut because when we compared to other coefficients, it did not seem as significant.
# lm.fit1=update(lm.fit,~.-cut)
# summary(lm.fit1) # Choi - RSE: 6949 on 119279 (=0.05825) Adjusted R-squared:  0.4187
#VK    stargazer(lm.fit,lm.fit1, title="Table 2: Multiple Linear Regression Results", out="LinearRegressionTable.html")
#when we compared the R squared with and without cut, the difference was very minimal, thereby proving our initial conclusion for cut
# Choi - I think we still need to include 'cut' since RSE is smaller and R-squared is higher in lm.fit than in lm.fit1

#test for collinearity
?vif()
vif(lm.fit)
#VK   ??? How to interpret GVIF^(1/(2*dF))
#VK   ??? If collinearity might exist, is that a good justification not to use the multiple linear regression model
#VK   ??? If cannot use multiple linear regression model, what can we use, knowing that there might be collinearity
# Choi:
# Reference: Day 7 Multi-linear Regression Slide 31, 43-46, Day 8 pdf (Lab2) slide 25 
# Reference: Lab 2 R file - 3.6.3 Multiple Linear Regression
# Day 8 pdf (Lab2) slide 25
# https://rforpoliticalscience.com/2020/08/03/check-for-multicollinearity-with-the-car-package-in-r/
          # To check for multicollinearity problem in our model, we need the vif() function from the car package in R. 
          # VIF stands for variance inflation factor. 
          # It measures how much the variance of any one of the coefficients is inflated due to multicollinearity in the overall model.
          # As a rule of thumb, a vif score over 5 is a problem. 
          # A score over 10 should be remedied and you should consider dropping the problematic variable from the regression model or creating an index of all the closely related variables.
vif(lm.fit)          
#              GVIF Df GVIF^(1/(2*Df))
# shape    1.456078  9        1.021094
# carat    1.425661  1        1.194011
# cut      1.363924  4        1.039558
# color    1.220264  6        1.016727
# clarity  1.237186  7        1.015319
# report  23.952182  3        1.697817 -- 23.95 > 10 --> 'report' should be excluded from lm.fit
# type    21.574678  1        4.644855 -- 21.57 > 10 --> 'type' should be excluded from lm.fit

lm.fit1=update(lm.fit,~.-report) 
vif(lm.fit1)
#GVIF Df GVIF^(1/(2*Df))
#shape   1.425490  9        1.019891
#carat   1.401832  1        1.183990
#cut     1.317234  4        1.035042
#color   1.196416  6        1.015056
#clarity 1.156101  7        1.010415
#type    1.395373  1        1.181259
summary(lm.fit1)
# Choi - RSE: 6954 on 119278 (=0.05830) Adjusted R-squared:  0.4179

#diagnostic Plots for the Multiple Linear Regression Model
par(mfrow=c(2,2))
plot(lm.fit)
#VK   ??? Should we remove high-leverage points and re-run regression model

#studentized residual plot and leverage statistics
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

#interaction terms
lm.fit2=lm(price~.^2,data=b_diamond)
summary(lm.fit2)
#VK    stargazer(lm.fit2, title="Table 3: Regression Results From Adding Interaction Terms", out="InteractionTermsTable.html")
#VK   ??? How to interpret this table and use for multiple linear regression model? Many of the interaction terms are significant

#polynomials
lm.fit3=lm(price~.+I(carat^2),data=b_diamond)
summary(lm.fit3)
anova(lm.fit,lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)
#VK    stargazer(lm.fit3, title="Table 3: Regression Results From Adding Interaction Terms", out="InteractionTermsTable.html")
#VK   ??? How to interpret this table? Many of the interaction terms are significant
#VK   ??? If this is good, do we need to test adding higher order polynomials

#VK   ???log transformation
#VK   summary(lm(medv~log(rm),data=Boston))

#VK   Not sure if the code below is accurate: logistic regression
b_diamond_log=b_diamond
b_diamond_log$price_high = rep("Low", nrow(b_diamond_log))
b_diamond_log$price_high[b_diamond_log$price>3490] = "High"
#VK   ??? 3490 is the third quartile amount - using this arbitrarily since this is the average price spent on a diamond ring
b_diamond_log$price_high=as.factor(b_diamond_log$price_high)
glm.fits=glm(price_high~.,data=b_diamond_log,family=binomial)
summary(glm.fits)

#VK   Need to do cross-validation, bootstrap, etc. to calculate test error and accuracy rates

# Choi Assessing Model Accuracy - 1. Residual Standard Error (RSE) 2. R-squared (R^2)
# Choi Reference: Day 6 Linear Regression pdf slide 18



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


pairs(~price+color+clarity+cut+type+report+shape+carat,b_diamond)
pairs(b_diamond)
