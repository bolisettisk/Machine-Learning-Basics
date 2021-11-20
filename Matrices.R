library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)
library(broom)
data("mnist_27")

if(!exists("mnist")) mnist <- read_mnist()
cat("\014")
class(mnist$train$images)

x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

length(x[,1])

grid <- matrix(x[3,], 28, 28)

image(1:28, 1:28, grid)
image(1:28, 1:28, grid[,28:1])
