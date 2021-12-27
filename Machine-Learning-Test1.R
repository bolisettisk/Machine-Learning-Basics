library(dslabs)
library(tidyverse)
library(caret)
library(dplyr)
library(ggplot2)

data(tissue_gene_expression)

cat("\014")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)
str(tissue_gene_expression)

# Q4.1.1.1
cat("\014")
d <- dist(tissue_gene_expression$x)

# Q4.1.1.2
cat("\014")
d1 <- as.matrix(d)[c(1,39,73),c(2,40,74)]
d1

# Q4.1.1.3
cat("\014")
image(as.matrix(d))

# Q4.1.2.1
cat("\014")
data(heights)
y <- heights$sex
x <- heights$height
# set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  F_meas(data = y_hat, reference = factor(test_set$sex))
})
max(F_1)
ks[which.max(F_1)]
min(k[F_1 == max(F_1)]) # this yields a wrong answer


# Q4.1.2.2
cat("\014")
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_x <- x[test_index, ]
train_x <- x[-test_index,]
test_y <- y[test_index]
train_y <- y[-test_index]

train_data <- data.frame(x = train_x, y = train_y)
test_data <- data.frame(x = test_x, y = test_y)

k <- seq(1, 11, 2)



cat("\014")
accuracy1 <- map_df(k, function(z){
  fit <- knn3(y ~ ., data = train_data, k = z)
  y_hat <- predict(fit, test_data, type = "class")
  confusionMatrix(y_hat, test_data$y)$overall["Accuracy"]
})
set.seed(1)
accuracy2 <- sapply(k, function(z){
  fit1 <- knn3(y ~ ., data = train_data, k = z)
  y_hat1 <- predict(fit1, test_data, type = "class")
  confusionMatrix(y_hat1, test_data$y)$overall["Accuracy"]
})
cat("\014")
data <- data.frame(k = k, sply = accuracy2, mapdf = accuracy1$Accuracy)
data






