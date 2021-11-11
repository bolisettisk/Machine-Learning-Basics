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

cat("\014")
p <- 0.9
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, prob = c(p, 1-p)) %>% factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC Curve

cat("\014")
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p,1-p)) %>% factor(levels = c("Female", "Male"))
  list(method = "Guessing", FPR = 1 - specificity(y_hat, test_set$sex), TPR = sensitivity(y_hat, test_set$sex))
})

guessing %>% qplot(FPR, TPR, data = ., xlab = "1 - Specificity", ylab = "Sensitivity")

cat("\014")
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% factor(levels = c("Female", "Male"))
  list(method = "Height cutoff", FPR = 1-specificity(y_hat, test_set$sex), TPR = sensitivity(y_hat, test_set$sex))
})

height_cutoff %>% qplot(FPR, TPR, data = ., xlab = "1 - Specificity", ylab = "Sensitivity")

bind_rows(guessing, height_cutoff) %>% ggplot(aes(FPR, TPR, color = method)) + geom_point() + geom_line() + xlab("1 - Specificity") + ylab("Sensitivity")

# Parecision-Recall curve

cat("\014")
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p,1-p)) %>% factor(levels = c("Female", "Male"))
  list(method = "Guess", precision = precision(y_hat, test_set$sex), recall = sensitivity(y_hat, test_set$sex))
})

cat("\014")
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% factor(levels = c("Female", "Male"))
  list(method = "Height cutoff", precision = precision(y_hat, test_set$sex), recall = sensitivity(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>% ggplot(aes(recall, precision, color = method)) + geom_point() + geom_line()

# change positive to male and negative to female

cat("\014")
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p,1-p)) %>% factor(levels = c("Male", "Female"))
  list(method = "Guess", precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")), recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")))
})

cat("\014")
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% factor(levels = c("Male", "Female"))
  list(method = "Height cutoff", precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")), recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")))
})

bind_rows(guessing, height_cutoff) %>% ggplot(aes(recall, precision, color = method)) + geom_point() + geom_line()
