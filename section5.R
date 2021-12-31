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


library(randomForest)
fit <- randomForest(margin~., data = polls_2008)
plot(fit)

polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")


train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

cat("\014")

train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

getModelInfo("knn")
modelLookup("knn")

train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE)


train_knn <- train(y ~ ., method = "knn",
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune
train_knn$finalModel

confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn",
                      data = mnist_27$train,
#                       tuneGrid = data.frame(k = seq(9, 71, 2)),
#                       trControl = control)
# ggplot(train_knn_cv, highlight = TRUE)
# 
# 
# train_knn$results %>% 
#   ggplot(aes(x = k, y = Accuracy)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(x = k, 
#                     ymin = Accuracy - AccuracySD,
#                     ymax = Accuracy + AccuracySD))
# 
# plot_cond_prob <- function(p_hat=NULL){
#   tmp <- mnist_27$true_p
#   if(!is.null(p_hat)){
#     tmp <- mutate(tmp, p=p_hat)
#   }
#   tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
#     geom_raster(show.legend = FALSE) +
#     scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
#     stat_contour(breaks=c(0.5),color="black")
# }
# plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])

# install.packages("gam")
# modelLookup("gamLoess")
# 
# grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)
# 
# train_loess <- train(y ~ ., 
#                      method = "gamLoess",
#                      tuneGrid=grid,
#                      data = mnist_27$train)
# ggplot(train_loess, highlight = TRUE)
# 
# confusionMatrix(data = predict(train_loess, mnist_27$test),
#                 reference = mnist_27$test$y)$overall["Accuracy"]
# 
# p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
# p1