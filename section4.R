library(tidyverse)
library(dslabs)
library(ggplot2)
options(warn=-1)


data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

if(!exists("mnist")) mnist <- read_mnist()

data("polls_2008")
qplot(day, margin, data = polls_2008)

span <- 7
fit <- with(polls_2008, ksmooth(day, margin, kernel = "box", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>% ggplot(aes(day, margin)) + geom_point(size = 3, aplha = 0.5, color = "grey") + geom_line(aes(day, smooth), color = "red")

span <- 7
fit <- with(polls_2008, ksmooth(day, margin, kernel = "normal", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>% ggplot(aes(day, margin)) + geom_point(size = 3, aplha = 0.5, color = "grey") + geom_line(aes(day, smooth), color = "red")

cat("\014")
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

str(x)
str(y)

cat("\014")
avg <- rowMeans(x)
z <- tibble(labels = as.factor(y), row_averages = avg)
tibble(labels = as.factor(y), row_averages = avg) %>% qplot(labels, row_averages, data = ., geom = "boxplot")

cat("\014")

if(!exists("mnist")) mnist <- read_mnist()

set.seed(0)
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]
# y[1:3] = 7 7 2
sqrt(sum((x_1-x_2)^2))
sqrt(sum((x_1-x_3)^2))
sqrt(sum((x_2-x_3)^2))

sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

d <- dist(x)
class(d)

as.matrix(d)[1:3, 1:3]

image(as.matrix(d))

image(as.matrix(d)[order(y), order(y)])

d <- dist(t(x))
dim(as.matrix(d))

d_492 <- as.matrix(d)[492,]

image(1:28, 1:28, matrix(d_492, 28, 28))



cat("\014")
#logistic regression
library(caret)
mnist_27$test %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

fit_lm <- mnist_27$train %>% 
  mutate(y = ifelse(y == 7, 1, 0)) %>%
  lm(y ~ x_1 + x_2, data = .)
p_hat_lm <- predict(fit_lm, mnist_27$test)
y_hat_lm <- factor(ifelse(p_hat_lm > 0.5, 7, 2))
confusionMatrix(y_hat_lm, mnist_27$test$y)$overall["Accuracy"]


fit_knn <- knn3(y ~ ., data = mnist_27$train, k = 5)
y_hat_knn <- predict(fit_knn, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]
















