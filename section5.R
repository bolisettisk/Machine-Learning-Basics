library(tidyverse)
library(dslabs)
library(caret)
library(ggplot2)
data("olive")


names(olive)
table(olive$region)
cat("\014")
olive <- olive %>% select(-area)

fit <- train(region ~., method = "knn", tuneGrid = data.frame(k = seq(1, 15, 2)), data = olive)
ggplot(fit)