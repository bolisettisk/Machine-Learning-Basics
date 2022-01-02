library(tidyverse)
library(dslabs)
library(dplyr)
library(caret)
library(ggplot2)
library(rpart)
library(randomForest)
library(titanic)    # loads titanic_train data frame
library(rpart)
library(rpart.plot)


# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)


set.seed(42)
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
test_set <- titanic_clean[test_index,]
train_set <- titanic_clean[-test_index,]


# Q7
cat("\014")
set.seed(1)
fit_lda <- train(Survived ~ Fare, data = train_set, method = "lda")
y_hat_lda <- predict(fit_lda, test_set)
confusionMatrix(as.factor(y_hat_lda), test_set$Survived)$overall["Accuracy"]

set.seed(1)
fit_qda <- train(Survived ~ Fare, data = train_set, method = "qda")
y_hat_qda <- predict(fit_qda, test_set)
confusionMatrix(as.factor(y_hat_qda), test_set$Survived)$overall["Accuracy"]


# Q8
cat("\014")
set.seed(1)
fit_glm <- train(Survived ~ Age, data = train_set, method = "glm")
y_hat_glm <- predict(fit_glm, test_set)
confusionMatrix(as.factor(y_hat_glm), test_set$Survived)$overall["Accuracy"]

set.seed(1)
fit_glm1 <- train(Survived ~ Sex+Pclass+Fare+Age, data = train_set, method = "glm")
y_hat_glm1 <- predict(fit_glm1, test_set)
confusionMatrix(as.factor(y_hat_glm1), test_set$Survived)$overall["Accuracy"]

set.seed(1)
fit_glm2 <- train(Survived ~ ., data = train_set, method = "glm")
y_hat_glm2 <- predict(fit_glm2, test_set)
confusionMatrix(as.factor(y_hat_glm2), test_set$Survived)$overall["Accuracy"]


# Q9a Q9b
cat("\014")
set.seed(6)
fit_knn <- train(Survived ~ ., method = "knn", data = train_set, tuneGrid = data.frame(k = seq(3, 51, 2)))
ggplot(fit_knn, highlight = TRUE)
fit_knn$bestTune


# Q9c
cat("\014")
confusionMatrix(predict(fit_knn, test_set), test_set$Survived)$overall["Accuracy"]


# Q10
cat("\014")
set.seed(8)
control <- trainControl(method = "cv", number = 10, p = 0.9)
fit_knn_c <- train(Survived ~ ., method = "knn", data = train_set, tuneGrid = data.frame(k = seq(3, 51, 2)), trControl = control)
fit_knn$bestTune
confusionMatrix(predict(fit_knn_c, test_set), test_set$Survived)$overall["Accuracy"]


# Q11a
cat("\014")
set.seed(10)
fit_knn_rpart <- train(Survived ~ ., method = "rpart", data = train_set, tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
fit_knn_rpart$bestTune
confusionMatrix(predict(fit_knn_rpart, test_set), test_set$Survived)$overall["Accuracy"]


# Q11b
cat("\014")
rpart.plot(fit_knn_rpart$finalModel)


# Q11c
cat("\014")
dat <- data.frame(Survived = as.factor(1), Sex = "male", Pclass = 1, Age = 28, Fare = 8, SibSp = 0, Parch = 0, FamilySize = 1, Embarked = as.factor("S"))
predict(fit_knn_rpart, dat)



# Q12
cat("\014")
set.seed(14)
fit_knn_rf <- train(Survived ~ ., method = "rf", data = train_set, tuneGrid = data.frame(mtry = seq(1:7)), ntree = 100)
fit_knn_rf$bestTune
confusionMatrix(predict(fit_knn_rf, test_set), test_set$Survived)$overall["Accuracy"]
varImp(fit_knn_rf)







