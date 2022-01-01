library(tidyverse)
library(dslabs)
library(dplyr)
library(caret)
library(ggplot2)
library(rpart)
library(randomForest)


n <- 1000
sigma <- 0.25
set.seed(1) # if using R 3.5 or ealier
# set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

# Q1
fit <- rpart(y ~ ., data = dat)

# Q2
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# Q3
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) + geom_step(aes(x, y_hat), col = 2)

# Q4
fit <- randomForest(y ~ ., data = dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
  
# Q5
plot(fit)

# Q6
library(randomForest)
fit <- randomForest(y ~ ., data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
  
plot(fit)
