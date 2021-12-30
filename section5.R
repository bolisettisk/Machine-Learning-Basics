library(tidyverse)
library(dslabs)
library(dplyr)
library(caret)
library(ggplot2)
data("olive")


names(olive)
table(olive$region)
cat("\014")
olive <- olive %>% select(-area)

fit <- train(region ~., method = "knn", tuneGrid = data.frame(k = seq(1, 15, 2)), data = olive)
ggplot(fit)

dat <- olive %>% pivot_longer(-"region", names_to = "comp", values_to = "values")
dat %>% group_by(comp) %>% ggplot(aes(region,values, fill = region)) + geom_boxplot() + facet_wrap(~comp, scale = "free", ncol = 4)

olive %>% ggplot(aes(eicosenoic, linoleic, color = region)) + geom_point()

#-------------------------

data("polls_2008")
qplot(day, margin, data = polls_2008)










