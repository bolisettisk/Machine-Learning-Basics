library(caret)
library(tidyverse)
library(dslabs)

data(heights)

y <- heights$sex
x <- heights$height

set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)


test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% factor(levels = levels(test_set$sex))

cat("\014")
mean(y_hat == test_set$sex)

heights %>% group_by(sex) %>% summarise(mean(height), sd(height))


y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))

mean(y == y_hat)