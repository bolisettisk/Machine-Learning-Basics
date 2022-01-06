library(tidyverse)
library(dslabs)
library(dplyr)
library(caret)
library(ggplot2)
library(rpart)
library(matrixStats)

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














