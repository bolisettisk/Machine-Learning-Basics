library(tidyverse)
library(dslabs)
library(dplyr)
library(caret)
library(ggplot2)
library(rpart)
data("olive")


names(olive)
table(olive$region)
cat("\014")
olive <- olive %>% select(-area)

fit <- train(region ~., method = "knn", tuneGrid = data.frame(k = seq(1, 15, 2)), data = olive)
ggplot(fit)

dat <- olive %>% pivot_longer(-"region", names_to = "Composition", values_to = "Vals")
dat %>% group_by(Composition) %>% ggplot(aes(region,Vals, fill = region)) + geom_boxplot() + facet_wrap(~Composition, scale = "free", ncol = 4)

olive %>% ggplot(aes(eicosenoic, linoleic, color = region)) + geom_point()

#-------------------------

data("polls_2008")
qplot(day, margin, data = polls_2008)

fit <- rpart(margin ~ ., data = polls_2008)

plot(fit, margin = 0.1)
text(fit, cex = 0.75)

polls_2008 %>% mutate(y_hat = predict(fit)) %>% ggplot() + geom_point(aes(day, margin)) + geom_step(aes(day, y_hat), col = "red")


fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% mutate(y_hat = predict(fit)) %>% ggplot() + geom_point(aes(day, margin)) + geom_step(aes(day, y_hat), col = "red")
plot(fit, margin = 0.005)
text(fit, cex = 0.5)

# use cross validation to choose cp
library(caret)
train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = polls_2008)
ggplot(train_rpart)

# access the final model and plot it
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

polls_2008 %>%
  mutate(y_hat = predict(train_rpart)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# prune the tree
pruned_fit <- prune(fit, cp = 0.05)
plot(pruned_fit, margin = 0.1)
text(pruned_fit, cex = 0.75)

data("mnist_27")
train_rpart <- train(y ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)), data = mnist_27$train)

plot(train_rpart)

y_hat <- predict(train_rpart, mnist_27$test)
confusionMatrix(y_hat, mnist_27$test$y)$overall["Accuracy"]





