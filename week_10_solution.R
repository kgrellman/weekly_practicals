install.packages("tidyverse")
install.packages("geometry")
install.packages("factoextra")
Yes
library(geometry)
library(tidyverse)
library(factoextra)
install.packages("moderndive")
library(moderndive)
library(readr)

# Question 1
# Generate 4 variables W, X, Y, and Z
# X and Y are independent
set.seed(22)
x = rnorm(30, 0, 2)
y = rnorm(30, 5, 5)
error = rnorm(30, 1, 2)

# Question 2
# W and X should have a mild correlation ( < 0.5)
w = 0.2 * x + error

# Question 3
# Y and Z should have a mild correlation ( > 0.9)
z = 1.2 * y + error

# Question 4
# Generate a variable outcome as a linear combination of W, X, Y and Z
# outcome =𝛽0+𝛽1×𝑊+𝛽2×𝑋+𝛽3×𝑌+𝛽4×𝑍
outcome = .1 + 2.1 * w + 1.2 * x + .5 * y + 1.5 * z
outcome
df_wxyz = data.frame(w, x, y, z)
df_wxyz

# Question 5
# Model your outcome
lm_xy = lm(x~y, df_wxyz)
lm_xw = lm(x~w, df_wxyz)
lm_yz = lm(y~z, df_wxyz)
summary(lm_xy)
summary(lm_xw)
summary(lm_yz)

# Question 6
# Use PCA to reduce dimensionality of the set
pca_wxyz = prcomp(df_wxyz, scale=TRUE)
summary(pca_wxyz)
str(pca_wxyz)
pca_wxyz$sdev^2 / sum(pca_wxyz$sdev^2)

# Question 7
# Use bi-plot to visualize the contributions of your initial variables
fviz_pca_biplot(pca_wxyz)

# Question 8
# How efficient is the new lower-dimensional space representation at predicting the outcome? Do your results match the model params?

# Dim1 and Dim2 capture 50.7% and 35.1% of the variability, respectively. Yes, the results match the model params.

