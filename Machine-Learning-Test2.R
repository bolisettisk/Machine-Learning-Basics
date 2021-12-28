library(dslabs)
library(tidyverse)
library(caret)
library(dplyr)
library(ggplot2)


# Q4.2.1.1
cat("\014")
set.seed(1996) #if you are using R 3.5 or earlier
# set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

cat("\014")
fit <- train(x_subset, y, method = "glm")
fit$results


# Q4.2.1.2
cat("\014")
# install.packages("BiocManager")
# BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value


# Q4.2.1.3
cat("\014")
ind <- which(tt$p.value < 0.01)
length(ind)
# or
sum(pvals < 0.01)


# Q4.2.1.4
cat("\014")
x_subset <- x[ ,ind]
fit <- train(x_subset, y, method = "glm")
fit$results


# Q4.2.1.5
cat("\014")
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)


# Q4.2.1.6
cat("\014")
# In the previous exercises, we see that despite the fact that x and y are completely independent, we were able to predict y with accuracy higher than 70%. We must be doing something wrong then.
# What is it?
# Ans: We used the entire dataset to select the columns used in the model.


# Q4.2.1.7
cat("\014")
data(tissue_gene_expression)
k = seq(1,7,2)
fit <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn", tuneGrid = data.frame(k = k))


# Q4.2.2.1
cat("\014")
data(mnist_27)
set.seed(1995) # if R 3.5 or earlier
# set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

cat("\014")
sum(indexes$Resample01 == 3)
sum(indexes$Resample01 == 4)
sum(indexes$Resample01 == 7)


# Q4.2.2.2
cat("\014")
y <- 0
y[1] <- sum(indexes$Resample01 == 3)
y[2] <- sum(indexes$Resample02 == 3)
y[3] <- sum(indexes$Resample03 == 3)
y[4] <- sum(indexes$Resample04 == 3)
y[5] <- sum(indexes$Resample05 == 3)
y[6] <- sum(indexes$Resample06 == 3)
y[7] <- sum(indexes$Resample07 == 3)
y[8] <- sum(indexes$Resample08 == 3)
y[9] <- sum(indexes$Resample09 == 3)
y[10] <- sum(indexes$Resample10 == 3)

sum(y)



# Q4.2.2.3
cat("\014")
y <- rnorm(100, 0, 1)
quantile(y, 0.75)
qnorm(0.75)

B <- 10000
set.seed(1)
dat <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

mean(dat)
sd(dat)


# Q4.2.2.4
cat("\014")
set.seed(1) # if R 3.5 or earlier
y <- rnorm(100, 0, 1)
B <- 10
#obtaining 10 bootstraps
set.seed(1)
dat <- replicate(B, {
  y <- sample(y, 100, replace = TRUE)
  quantile(y, 0.75)
})

mean(dat)
sd(dat)


# Q4.2.2.5
cat("\014")
set.seed(1) # if R 3.5 or earlier
y <- rnorm(100, 0, 1)
B <- 10000
#obtaining 10 bootstraps
set.seed(1)
dat <- replicate(B, {
  y <- sample(y, 100, replace = TRUE)
  quantile(y, 0.75)  
})

mean(dat)
sd(dat)


# Q4.2.2.5
cat("\014")
# When doing bootstrap sampling, the simulated samples are drawn from the empirical distribution of the original data. True or False: The bootstrap is particularly useful in situations when we do not have access to the distribution or it is unknown.
# Ans: True

















