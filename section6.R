library(tidyverse)
library(dslabs)
library(dplyr)
library(caret)
library(ggplot2)
library(rpart)
library(matrixStats)
library(randomForest)
library(Rborist)

if(!exists("mnist")) mnist <- read_mnist()

cat("\014")
names(mnist)
dim(mnist$train$images)
dim(as.matrix(mnist$train$labels))
class(mnist$train$labels)
table(mnist$train$labels)

cat("\014")
set.seed(1990)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index, ]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index, ]
y_test <- factor(mnist$test$labels[index])

cat("\014")
sds <- colSds(x)
qplot(sds, bins = 256)

nzv <- nearZeroVar(x)

# rafalib::mypar()
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

# Commented since it takes to too long to execute
# control <- trainControl(method = "cv", number = 10, p = 0.9)
# train_knn <- train(x[, col_index], y, method = "knn", tuneGrid = data.frame(k = c(3, 5, 7)), trControl = control)
# 
# cat("\014")
# train_knn

n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = 0.9)
train_knn <- train(x[index, col_index], y[index], method = "knn", tuneGrid = data.frame(k = c(3, 5, 7)), trControl = control)

cat("\014")
train_knn # Result: Accuracy was used to select the optimal model using the largest value. The final value used for the model was k = 3.

fit_knn <- knn3(x[, col_index], y, k = 3)
y_hat_knn <- predict(fit_knn, x_test[, col_index], type = "class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

cat("\014")
cm$byClass[,1:2]


cat("\014")
control <- trainControl(method = "cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5), predFixed = c(10, 15, 25, 35, 50))

# Commented since it takes to too long to execute
# train_rf <- train(x[ , col_index], y, method = "Rborist", nTree = 50, trControl = control, tuneGrid = grid, nSamp = 5000)

ggplot(train_rf)
train_rf$bestTune

fit_rf <- Rborist(x[ , col_index], y, nTree = 1000, minNode = train_rf$bestTune$minNode, predFixed = train_rf$bestTune$predFixed)

cat("\014")
y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

rafalib::mypar(3,4)
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste("Our prediction:", y_hat_rf[i]),
        xaxt="n", yaxt="n")
}







