library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)
library(broom)
library(matrixStats)


cat("\014")
#Q1
x <- matrix(rnorm(100*10), 100, 10)
dim(x)


cat("\014")
#Q2
dim(x)
nrow(x)
ncol(x)


cat("\014")
#Q3
x <- x + seq(nrow(x))
x <- sweep(x, 1, 1:nrow(x),"+")


cat("\014")
#Q4
x <- sweep(x, 2, 1:ncol(x), FUN = "+")


cat("\014")
#Q5
rowMeans(x)
colMeans(x)


cat("\014")
#Q6
data("mnist_27")
if(!exists("mnist")) mnist <- read_mnist()
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]
xm <- mean(x > 50 & x < 205)
xm
# filt <- function(x){
#   ifelse((x > 50 & x < 205), 1, 0)
# }
# xf <- filt(x)
# xf <- rowMeans(xf)
# tibble(labels = as.factor(y), rows = xf) %>% qplot(as.factor(y), xf, data = ., geom = "boxplot")
# xf1 <- rowMeans(x > 50 & x < 205)
# tibble(labels = as.factor(y), rows = xf1) %>% qplot(as.factor(y), xf, data = ., geom = "boxplot")




