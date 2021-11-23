install.packages("lubridate")
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(car)
setwd('/Users/Kat/Documents/1_UHawaii/MBIO 612/GitHub')
data = tibble(read.csv("data/YERockfish.csv"))
View(data)

# normalize date format
data1 = data %>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y"))
head(data1)

nrow(data1)      
# 158 lines in data set 

# plot the count of observations per year
counts = data1 %>%
  group_by(year = year(data1$date)) %>%
  summarise(observations = n())
counts
ggplot(counts) +
  geom_point(mapping = aes(year, observations))

# remove year(s) for which there are less than 5 entries
filter_counts = counts %>%
  filter(observations>5)
filter = filter(observations>5)

# make sure number of observations<number of entries (158)
sum(filter_counts$observations)
# 154<158
data2 = data1 %>%
  mutate(year=year(date)) %>%
  filter(year!=2004) %>%
  filter(year!=2008)
View(data2)
## removed entries for years 2004 and 2008

# prep data to model fish maturity using the fish length
## remove NA values
length_mat = data2 %>%
  select(length, maturity) %>%
  filter(!is.na(maturity))
View(length_mat)
# convert 'maturity' characters to integers; Immature=0, Mature=1
length_mat$maturity = ifelse(length_mat$maturity=="Immature",0,1)
length_mat$maturity = as.factor(length_mat$maturity)
num_maturity = as.numeric(levels(length_mat$maturity))[length_mat$maturity]

# predict maturity from length data
log_reg_model = glm(data=length_mat, maturity~length, family="binomial")
summary(log_reg_model)
# plot the data and show model fit
beta_0 = log_reg_model$coefficients[1]
beta_1 = log_reg_model$coefficients[2]
x_axis = seq(min(length_mat$length)-3, max(length_mat$length)+3, 0.05)
g_x = 1 / (1+exp(-(beta_0 + beta_1 * x_axis)))
ggplot()+ 
  geom_point(aes(x=length_mat$length, y=num_maturity)) + 
  geom_line(aes(x_axis, g_x)) +
  xlab("Length") +
  ylab("Maturity") +
  theme(text = element_text(size = 14))
# based on visual approximation, length of picking a mature fish with 50% probability is ~38mm

# find the length at which the probability of picking a mature fish is 0.5
## visual approximation shows length= ~38cm
## full disclosure - I found the below function from the 'Maturity Analyses' blog
lrPerc = function(cf,p) (log(p/(1-p))-cf[[1]])/cf[[2]]
L50 = lrPerc(coef(log_reg_model), 0.5)
L50
# length = 38.76736cm

# add era column to dataset
data3 = data2 %>%
  filter(!is.na(maturity)) %>%
  mutate(era = ifelse(year<2002,"pre 2002", "2002 and after"))
view(data3)
# convert 'maturity' characters to integers; Immature=0, Mature=1
data3$maturity = ifelse(data3$maturity=="Immature",0,1)
data3$maturity = as.factor(data3$maturity)
num_data3 = as.numeric(levels(data3$maturity))[data3$maturity]
# convert 'era' characters to integers; pre 2002=0, post 2002=1
data3$era = ifelse(data3$era=="pre 2002",0,1)
data3$era = as.factor(data3$era)
num_data3 = as.numeric(levels(data3$era))[data3$era]
head(data3)

# build a logistic regression for maturity as an outcome using era and length as predictive variables
model_data = data3 %>%
  select(length, maturity, era)
head(model_data)
log_reg_model2 = glm(data=model_data, maturity~length*era, family="binomial")
summary(log_reg_model2)
# run ANOVA
aov_results = aov(formula = maturity~length*era, data=model_data)
summary(aov_results)
# based on the ANOVA, 'era' does not appear to be a significant variable for predicting maturity

