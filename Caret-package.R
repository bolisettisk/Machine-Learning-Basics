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


cat("\014")

cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

cat("\014")
best_cutoff <- cutoff[which.max(accuracy)]

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels(test_set$sex))

y_hat <- factor(y_hat)

mean(y_hat == test_set$sex)

cat("\014")

table(predicted = y_hat, actual = test_set$sex)

test_set %>% mutate(y_hat = y_hat) %>% group_by(sex) %>% summarise(accuracy = mean(y_hat == sex))

cat("\014")
cm <- confusionMatrix(data = y_hat, reference = test_set$sex)
cm$overall["Accuracy"]

cat("\014")

cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% ggplot(aes(cutoff, F_1)) + geom_point() + geom_line()

max(F_1)
best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels(test_set$sex))
confusionMatrix(data = y_hat, reference = test_set$sex)

