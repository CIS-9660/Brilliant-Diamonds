#Data Summary Statistics 
install.packages("gtsummary")
install.packages("qpcR")
library(gtsummary)
library(ggplot2)
library(GGally)
library(data.table)
library(modelr)
library(stargazer)
library(qpcR)
library(tidyverse)

options(scipen = 999) #disabling scientific notation in R
b_diamond=read.csv("BDiamond.csv", header=T) #reading csv
#remove all arbitrary columns to ensure cleaner analysis
b_diamond = select(b_diamond, -url, -id,-date_fetched)

#Summary of quantitative variables
stargazer(b_diamond[1:2],title="Table 1: Summary Results", 
          out="QuantSummaryTable.html", digits=2)

#table summary of all the qualitative variables 
diamond_qua = b_diamond %>% select(shape, cut, color, clarity, report, type)
diamond_qua %>% tbl_summary(
  statistic = list(all_continuous() ~ "{mean} {sd}",
                   all_categorical() ~ "{n} / {N} ({p}%)"),
  digits = all_continuous() ~ 2) %>%
  bold_labels()



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
diamond_cut = b_diamond %>%
  group_by(cut) %>%
  summarize(m = mean(price)) %>%
  ungroup() %>% #we can also try to graph it
  ggplot(aes(cut, m)) +
  geom_bar(stat = "identity")
diamond_cut

# mean price based on color 
diamond_color = b_diamond %>%
  group_by(color) %>%
  summarize(m = mean(price)) %>%
  ungroup() %>% #we can also try to graph it
  ggplot(aes(color, m)) +
  geom_bar(stat = "identity")
diamond_color

# mean price based on clarity 
diamond_clarity = b_diamond %>%
  group_by(clarity) %>%
  summarize(m = mean(price)) %>%
  ungroup()%>% #we can also try to graph it
  ggplot(aes(clarity, m)) +
  geom_bar(stat = "identity")
diamond_clarity

# Shape 
diamond_shape = b_diamond %>%
  group_by(shape) %>%
  summarise(m = mean(price)) %>%
  ungroup() %>%
  ggplot(aes(shape, m)) +
  geom_bar(stat = "identity")
diamond_shape

# Reports 
diamond_report = b_diamond %>%
  group_by(report) %>%
  summarise(m = mean(price)) %>%
  ungroup() %>%
  ggplot(aes(report, m)) +
  geom_bar(stat = "identity")
diamond_report

# Type - Lab vs. natural  
diamond_type = b_diamond %>%
  group_by(type) %>%
  summarise(m = mean(price)) %>%
  ungroup() %>%
  ggplot(aes(type, m)) +
  geom_bar(stat = "identity")
diamond_type

grid.arrange(diamond_cut, diamond_clarity, diamond_color,
             diamond_shape, diamond_report, diamond_type)
# Combining tables of all means of all qualitative variables in dataset 
# into one big table 
diamond_overall = qpcR:::cbind.na(diamond_clarity, diamond_color, diamond_cut,
                                  diamond_shape, diamond_report, diamond_type)
diamond_overall[is.na(diamond_overall)] <- " "
diamond_overall
#https://stackoverflow.com/questions/3699405/how-to-cbind-or-rbind-different-lengths-vectors-without-repeating-the-elements-o



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


#This is graph for each variable just summarize how many each variable has. 
install.packages('gridExtra')
library(gridExtra)
plot_cut = ggplot(b_diamond, aes(x=cut, y=price)) + geom_bar(stat = "identity")
plot_color = ggplot(b_diamond, aes(x=color, y=price)) + geom_bar(stat = "identity")
plot_clarity = ggplot(b_diamond, aes(x=clarity, y=price)) + geom_bar(stat = "identity")
plot_shape = ggplot(b_diamond, aes(x=shape, y=price)) + geom_bar(stat = "identity")
plot_report = ggplot(b_diamond, aes(x=report, y=price)) + geom_bar(stat = "identity")
plot_type = ggplot(b_diamond, aes(x=type, y=price)) + geom_bar(stat = "identity")
grid.arrange(plot_cut, plot_color, plot_clarity, plot_shape, 
             plot_report, plot_type)
