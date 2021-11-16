library(tidyverse)
library(caret)


#Q4

set.seed(1) # if using R 3.5 or earlier
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1) # if using R 3.5 or earlier
# set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
rep_dat <- replicate(n, {
  index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  test <- dat %>% slice(index)
  train <- dat %>% slice(-index)
  fit <- lm(y ~ x, data = train)
  test_p <- fit$coefficients[1] + fit$coefficients[2]*test$x
  # test_p <- predict(fit, newdata = test)
  sqrt(mean((test_p - test$y)^2))
})
mean(rep_dat)
sd(rep_dat)


cat("\014")
#Q6

set.seed(1) # if using R 3.5 or earlier
# set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat) #Note that y is correlated with both x_1 and x_2 but the two predictors are independent of each other, as seen by cor(dat).

set.seed(1) # if using R 3.5 or earlier
index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test <- dat %>% slice(index)
train <- dat %>% slice(-index)
# fit_x1 <- lm(y ~ x_1, data = train)
# fit_x2 <- lm(y ~ x_2, data = train)
fit_x12 <- lm(y ~ x_1+x_2, data = train)
# test_x1 <- fit_x1$coefficients[1] + fit_x1$coefficients[2]*test$x_1
# test_x2 <- fit_x2$coefficients[1] + fit_x2$coefficients[2]*test$x_2
test_x12 <- fit_x12$coefficients[1] + fit_x1$coefficients[2]*test$x_1 + fit_x12$coefficients[3]*test$x_2
cat("\014")
# fit_x1
# sqrt(mean((test_x1 - test$y)^2))
# fit_x2
# sqrt(mean((test_x2 - test$y)^2))
fit_x12
sqrt(mean((test_x12 - test$y)^2))


cat("\014")
#Q7



set.seed(1) # if using R 3.5 or earlier
# set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1) # if using R 3.5 or earlier
index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test <- dat %>% slice(index)
train <- dat %>% slice(-index)
# fit_x1 <- lm(y ~ x_1, data = train)
# fit_x2 <- lm(y ~ x_2, data = train)
fit_x12 <- lm(y ~ x_1+x_2, data = train)
# test_x1 <- fit_x1$coefficients[1] + fit_x1$coefficients[2]*test$x_1
# test_x2 <- fit_x2$coefficients[1] + fit_x2$coefficients[2]*test$x_2
test_x12 <- fit_x12$coefficients[1] + fit_x1$coefficients[2]*test$x_1 + fit_x12$coefficients[3]*test$x_2
cat("\014")
# fit_x1
# sqrt(mean((test_x1 - test$y)^2))
# fit_x2
# sqrt(mean((test_x2 - test$y)^2))
fit_x12
sqrt(mean((test_x12 - test$y)^2))


