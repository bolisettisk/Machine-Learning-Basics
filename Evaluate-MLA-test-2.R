library(caret)
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggrepel)
library(lubridate)

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

#Q7
set.seed(2) # if using R 3.5 or earlier
# set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

#Q8
test_Species<- test$Species %>% factor(levels=c("virginica", "versicolor"))
train_Species<- train$Species %>% factor(levels=c("virginica", "versicolor"))
#Sepal.Length
cutoff <- seq(min(train$Sepal.Length), max(train$Sepal.Length), 0.1)
accuracy1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% factor(levels=c("versicolor", "virginica"))
  mean(y_hat == train_Species)
})
best_cutoff1 <- cutoff[which.max(accuracy1)]
#Sepal.Width 
cutoff <- seq(min(train$Sepal.Width), max(train$Sepal.Width), 0.1)
accuracy2 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% factor(levels=c("versicolor", "virginica"))
  mean(y_hat == train_Species)
})
best_cutoff2 <- cutoff[which.max(accuracy2)]
#Petal.Length
cutoff <- seq(min(train$Petal.Length), max(train$Petal.Length), 0.1)
accuracy3 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% factor(levels=c("versicolor", "virginica"))
  mean(y_hat == train_Species)
})
best_cutoff3 <- cutoff[which.max(accuracy3)]
#Petal.Width
cutoff <- seq(min(train$Petal.Width), max(train$Petal.Width), 0.1)
accuracy4 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% factor(levels=c("versicolor", "virginica"))
  mean(y_hat == train_Species)
})
best_cutoff4 <- cutoff[which.max(accuracy4)]
c(max(accuracy1), max(accuracy2), max(accuracy3), max(accuracy4))

#Q9
cat("\014")
cutoff <- best_cutoff3
y_hat <- ifelse(test$Petal.Length > cutoff, "virginica", "versicolor") %>% factor(levels=c("versicolor", "virginica"))
mean(y_hat == test_Species)

#Q10
cat("\014")
#Sepal.Length
cutoff <- seq(min(test$Sepal.Length), max(test$Sepal.Length), 0.1)
accuracy1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor") %>% factor(levels=c("versicolor", "virginica"))
  mean(y_hat == test_Species)
})
best_cutoff1 <- cutoff[which.max(accuracy1)]
#Sepal.Width 
cutoff <- seq(min(test$Sepal.Width), max(test$Sepal.Width), 0.1)
accuracy2 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Width > x, "virginica", "versicolor") %>% factor(levels=c("versicolor", "virginica"))
  mean(y_hat == test_Species)
})
best_cutoff2 <- cutoff[which.max(accuracy2)]
#Petal.Length
cutoff <- seq(min(test$Petal.Length), max(test$Petal.Length), 0.1)
accuracy3 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") %>% factor(levels=c("versicolor", "virginica"))
  mean(y_hat == test_Species)
})
best_cutoff3 <- cutoff[which.max(accuracy3)]
#Petal.Width
cutoff <- seq(min(test$Petal.Width), max(test$Petal.Width), 0.1)
accuracy4 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor") %>% factor(levels=c("versicolor", "virginica"))
  mean(y_hat == test_Species)
})
best_cutoff4 <- cutoff[which.max(accuracy4)]
c(max(accuracy1), max(accuracy2), max(accuracy3), max(accuracy4))

#Q11
cat("\014")
plot(iris,pch=21,bg=iris$Species)

#Petal.Length
cutoff <- seq(min(train$Petal.Length), max(train$Petal.Length), 0.1)
accuracy3 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% factor(levels=c("versicolor", "virginica"))
  mean(y_hat == train_Species)
})
best_cutoff3 <- cutoff[which.max(accuracy3)]
#Petal.Width
cutoff <- seq(min(train$Petal.Width), max(train$Petal.Width), 0.1)
accuracy4 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% factor(levels=c("versicolor", "virginica"))
  mean(y_hat == train_Species)
})
best_cutoff4 <- cutoff[which.max(accuracy4)]

y_hat <- ifelse(test$Petal.Length > best_cutoff3 | test$Petal.Width > best_cutoff4, "virginica", "versicolor")
mean(y_hat == test_Species)