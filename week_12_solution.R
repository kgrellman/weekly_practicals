# worked with Aimee and Sophia

# Q1 Use the approach described in Chapter 6.5.1 of ISLR to select the best subset of explanatory variables to predict richness
## Note that the approach uses cross-validation
## You do not need to include interaction

library(tidyverse)
install.packages("pls")
library(pls)
library(leaps)
survey_data = read_delim("Data/RIKZ.txt", delim = "\t", 
                         escape_double = FALSE, trim_ws = TRUE)

# create column "richness"
species_cols=2:76
species_cols
counts = apply(survey_data[, species_cols] > 0, 1, sum)
survey_data["richness"] = counts
View(survey_data)

# create copy of data set without species data
survey_data_richness = survey_data[,-species_cols]
View(survey_data_richness)

# identify best model
reg_richness = regsubsets(richness ~ ., survey_data_richness, nvmax = 15)
summary_reg_richness = summary(reg_richness)
summary_reg_richness
# indicates that the best 2-variable model contains NAP and chalk
summary_reg_richness$rsq
# adjusted R^2 value is 34% with one variable, increases to 72.4% when all variables are included

# plot RSS for all the models
plot(summary_reg_richness$rss , xlab = " Number of Variables ",
        ylab = " RSS ", type = "l")
# plot adjust R^2 for all the models
plot(summary_reg_richness$adjr2 , xlab = " Number of Variables ",
        ylab = " Adjusted RSq ", type = "l")

# identify location of the max point of a vector
which.max(summary_reg_richness$adjr2)
# 6
points(6, summary_reg_richness$adjr2[6], col = " red ", cex = 2,
        pch = 20)
# model with the largest R^2 statistic is 6-variables 

# plot Cp and BIC statistics
plot(summary_reg_richness$cp, xlab = " Number of Variables ",
      ylab = "Cp", type = "l")
# indicate model with smallest statistic
which.min(summary_reg_richness$cp)
# 3
points(3, summary_reg_richness$cp[3], col = " red ", cex = 2,
        pch = 20)
which.min(summary_reg_richness$bic)
# 3
plot(summary_reg_richness$bic , xlab = " Number of Variables ",
      ylab = " BIC ", type = "l")
points (3, summary_reg_richness$bic[3], col = " red ", cex = 2,
        pch = 20)
# model with smallest statistic is 3-variables

# use plot() command to display the selected variables for the best model
?plot.regsubsets
plot(reg_richness, scale = "r2")
plot(reg_richness, scale = "adjr2")
plot(reg_richness, scale = "Cp")
plot(reg_richness, scale = "bic")
# model with lowest BIC contains exposure, salinity, and NAP
## use coef() to see coefficient estimates associated with this model
coef(reg_richness, 3)
# exposure, salinity, and NAP appear to be the best variables

# perform 'forward' and 'backward' stepwise selections
fwd_richness <- regsubsets (richness ~ ., survey_data_richness, nvmax = 15, method = "forward")
summary(fwd_richness)
# exposure and NAP appear to be most significant variables
bwd_richness <- regsubsets (richness ~ ., survey_data_richness, nvmax = 15, method = "backward")
summary(bwd_richness)
# NAP and temperature appear to be most significant variables

## WHY do backward and forward stepwise selections indicate such different variables? How would you know which model to go with?

# test for best 7-variable models
coef(reg_richness, 7)
coef(fwd_richness, 7)
coef(bwd_richness, 7)

# validation set approach
## split data into training set and test set
set.seed(1)
train = sample(c(TRUE, FALSE), nrow(survey_data_richness), replace = TRUE)
test = (!train)
# apply regsubsets() to training set to perform best subset selection
reg_best = regsubsets(richness ~., survey_data_richness[train, ], nvmax=15)
# make a model matrix from test data
test_mat = model.matrix(richness ~., survey_data_richness[train, ])

# for each size 'i', extract coefficients from reg_best for best model of that size
## multiply coefficients into appropriate columns of test model matrix to form predictions
val_errors = rep(NA, 14)
for (i in 1:14) {
    coefi = coef(reg_best, id = i)
    pred = test_mat[, names(coefi)] %*% coefi
    val_errors[i] = mean((survey_data_richness$richness[test] - pred)^2)
}
val_errors
which.min(val_errors)
# 8 variables minimize the error
coef(reg_best, 8)
# best variables are sample, exposure, salinity, temperature, NAP, grainsize, chalk, and sorting1

# create a 'predict' method using the previous function 
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars] %*% coefi
}

# run best subset selection on full dataset to select best 8-variable model
regfit.best = regsubsets(richness ~., survey_data_richness, nvmax=15)
coef(regfit.best, 8)
# best 8-variable model on full set is different than that on training set
## best variables are sample, angle1, angle2, temperature, NAP, penetrability, chalk, and sorting1

# choose among models of different sizes using cross-validation
## perform best subset selection within each of the k training sets
### create k folds
k=10
n=nrow(survey_data_richness)
set.seed(1)
# create matrix to store results
folds = sample(rep(1:k, length = n))
cv.errors = matrix(NA, k , 15, dimnames = list(NULL, paste(1:15)))
# write a function that performs cross-validation
## elements of folds that equal 'j' are in the test set
for (j in 1:k) {
  best.fit = regsubsets(richness ~., survey_data_richness[folds != j, ], nvmax=15)
  for (i in 1:14) {
    pred = predict(best.fit, survey_data_richness[folds ==j, ], id = i)
    cv.errors[j, i] = mean((survey_data_richness$richness[folds == j] - pred)^2)
  }
}
# average over matrix columns to find a vector for which ith element is the cross-validation error for the i-variable model
mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow = c(1,1))
plot(mean.cv.errors, type = "b")
# cross-validation selects a 2-variable model
## perform cross-validation on the full dataset to identify 2-variable model
reg.best = regsubsets(richness~., survey_data_richness, nvmax=15)
coef(reg.best, 2)
# best 2-variable model contains exposure and NAP as variables
