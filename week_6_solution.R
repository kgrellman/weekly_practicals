# Question 1

library(tidyverse)
setwd('/Users/user/Desktop/MBIO 612/GitHub')
# import 2018 dataset
read.csv("survey_2018.csv")
# define numeric types
data_18_tbl <- read_csv("survey_2018.csv",
                        col_types = cols(
                          transect_id = col_integer(),
                          coverage = col_double()
                        ))
print(data_18_tbl)

# import 2021 dataset
read.csv("re_sample_2021.csv")
# define numeric types
data_21_tbl <- read_csv("re_sample_2021.csv",
                        col_types = cols(
                          experiment_id = col_integer(),
                          transect_id = col_integer(),
                          coverage = col_double()
                        ))
print(data_21_tbl)
# calculate mean of coverage values
mean(data_21_tbl$coverage)

# subsample mean values from the 2018 data
set.seed(22)
n = 100000
sample_18 <- n %>%
  replicate(sample(data_18_tbl$coverage, 25)) %>%
  apply(2,mean)

ggplot() + 
  geom_histogram(aes(x=sample_18, y=..density..), bins = 100, alpha=0.1, color="black", size=0.05) +
  geom_density(aes(x=sample_18), color="black", size=1) +
# plot 2021 data mean value
  geom_vline(aes(xintercept=23.68), color="red") 

# calculate how many mean values are equal to/more than 2021 mean
sum(sample_18 >=23.68)/100000
# p-value is 0.00277
# 0.00277 < 0.05, test should be rejected
