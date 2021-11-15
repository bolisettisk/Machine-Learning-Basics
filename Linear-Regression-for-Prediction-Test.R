library(tidyverse)
library(caret)

# set.seed(1) # if using R 3.5 or earlier
# # set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# n <- 100
# Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
# dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
#   data.frame() %>% setNames(c("x", "y"))
# 
# #Q1
# 
# set.seed(1) # if using R 3.5 or earlier
# # set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# rep_dat <- replicate(n, {
#   index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
#   test <- dat %>% slice(index)
#   train <- dat %>% slice(-index)
#   fit <- lm(y ~ x, data = train)
#   test_p <- fit$coefficients[1] + fit$coefficients[2]*test$x
#   # test_p <- predict(fit, newdata = test)
#   sqrt(mean((test_p - test$y)^2))
# })
# mean(rep_dat)
# sd(rep_dat)

cat("\014")
# #Q2
# dat_fun <- function(n){
#   Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
#   dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>% data.frame() %>% setNames(c("x", "y"))
#   rep_dat <- replicate(n, {
#     index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
#     test <- dat %>% slice(index)
#     train <- dat %>% slice(-index)
#     fit <- lm(y ~ x, data = train)
#     test_p <- fit$coefficients[1] + fit$coefficients[2]*test$x
#     # test_p <- predict(fit, newdata = test)
#     sqrt(mean((test_p - test$y)^2))
#   })
#   c(mean(rep_dat), sd(rep_dat))
# }
# # n <- c(100, 500, 1000, 5000, 10000)
# n <- c(100, 500)
# set.seed(1) # if using R 3.5 or earlier
# # set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# sapply(n, dat_fun)

n <- c(100, 500, 1000, 5000, 10000)
set.seed(1) # if using R 3.5 or earlier
# set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>% data.frame() %>% setNames(c("x", "y"))
  rep_dat <- replicate(100, {
    index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    test <- dat %>% slice(index)
    train <- dat %>% slice(-index)
    fit <- lm(y ~ x, data = train)
    test_p <- fit$coefficients[1] + fit$coefficients[2]*test$x
    # test_p <- predict(fit, newdata = test)
    sqrt(mean((test_p - test$y)^2))
  })
  c(mean(rep_dat), sd(rep_dat))
})

