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







