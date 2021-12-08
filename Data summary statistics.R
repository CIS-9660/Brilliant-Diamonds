#Data Summary Statistics 
library(ggplot2)
library(GGally)
library(data.table)
library(modelr)
options(scipen = 999) #disabling scientific notation in R
b_diamond=read.csv("BDiamond.csv", header=T) #reading csv
#remove all arbitrary columns to ensure cleaner analysis
b_diamond = select(b_diamond, -url, -id,-date_fetched)

#Removed strong relationship between carat and price to see the actual relationship
#between cut and price, resid gives us view of price of diamond once the effect 
#of carat has been removed. 
mod = lm(log(price) ~ log(carat), data = b_diamond)
diamonds2 = diamonds %>%
  add_residuals(mod) %>%
  mutate(resid = exp(resid))
ggplot(data = diamonds2) + geom_point(aes(carat, resid))

#https://r4ds.had.co.nz/exploratory-data-analysis.html
#True relationship between residual and cut
ggplot(data = diamonds2) + geom_bar(aes(cut, resid), stat = "identity")

#True relationship between residual and color
ggplot(data = diamonds2) + geom_bar(aes(color, resid), stat = "identity")

#True relationship between residual and clarity 
ggplot(data = diamonds2) + geom_bar(aes(clarity, resid), stat = "identity")

# mean price based on cut 
b_diamond %>%
  group_by(cut) %>%
  summarize(m = mean(price)) %>%
  ungroup() #%>% we can also try to graph it
  #ggplot(aes(cut, m)) +
  #geom_point()

# mean price based on color 
b_diamond %>%
  group_by(color) %>%
  summarize(m = mean(price)) %>%
  ungroup()#%>% we can also try to graph it
  #ggplot(aes(color, m)) +
  #geom_point()

# mean price based on clarity 
b_diamond %>%
  group_by(clarity) %>%
  summarize(m = mean(price)) %>%
  ungroup()#%>% we can also try to graph it
  #ggplot(aes(clarity, m)) +
  #geom_point()

#bar graph based on different variables with mean price 
b_diamond %>%
  group_by(color, cut) %>%
  summarize(m = mean(price)) %>%
  ggplot(aes(color, m, group = cut, fill = cut)) +
  geom_bar(stat = "identity")
#https://bookdown.org/yih_huynh/Guide-to-R-Book/bar-graph.html

#scaling to make relationship between carat and price more clear
ggplot(b_diamond, aes(carat, price)) + geom_bin2d() + scale_x_log10() + 
  scale_y_log10() 



#ggplot(b_diamond, aes(x=cut, y=price)) + geom_bar(stat = "identity")
#ggplot(b_diamond, aes(x=color, y=price)) + geom_bar(stat = "identity")