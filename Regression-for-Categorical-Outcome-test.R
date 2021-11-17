library(dslabs)
library(caret)
library(dplyr)
library(ggplot2)
data("heights")
y <- heights$height



set.seed(2) #if you are using R 3.5 or earlier
# set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

cat("\014")

mu_1 <- seq(0, 3, len=25)
set.seed(1)
s_mu <- sapply(mu_1, make_data, n = 1000, p = 0.5, mu_0 = 0, sigma_0 = 1,  sigma_1 = 1, simplify = TRUE)
accu <- function(n){
  glm_fit <- glm(y~x, data = s_mu[,n]$train, family = "binomial")
  p_hat_logic <- predict(glm_fit, newdata = s_mu[,n]$test, type = "response")  
  y_hat_logic <- ifelse(p_hat_logic > 0.5, 1, 0) %>% factor
  confusionMatrix(y_hat_logic, s_mu[,n]$test$y)$overall[["Accuracy"]]
}
n <- seq(1,25)
set.seed(1)
accuracy <- sapply(n, accu)
plot(mu_1, accuracy)

