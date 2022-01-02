library(tidyverse)
library(dslabs)
library(dplyr)
library(caret)
library(ggplot2)
library(rpart)
library(randomForest)
library(titanic)    # loads titanic_train data frame


# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# Q1
cat("\014")
set.seed(42)
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
test_set <- titanic_clean[test_index,]
train_set <- titanic_clean[-test_index,]
cat("\014")
str(test_set)
str(train_set)
prop.table(table(train_set$Survived))


# Q2
cat("\014")
set.seed(3)
y_hat <- sample(c(0,1), length(test_index), replace = TRUE, prob = NULL) %>% factor(levels = levels(test_set$Survived))
mean(y_hat == test_set$Survived)


# Q3a
cat("\014")
train_set %>% filter(Sex == "female") %>% summarise(mean(Survived == 1))
train_set %>% filter(Sex == "male") %>% summarise(mean(Survived == 1))


# Q3b
cat("\014")
test_set %>% mutate(predt = ifelse(Sex == "female", 1, 0)) %>% summarise(mean(predt == Survived))


# Q4a
cat("\014")
train_set %>% group_by(Pclass) %>% summarise(prop.table(table(Survived)))



# Q4b
cat("\014")
test_set %>% mutate(predt = ifelse(Pclass == 1, 1, 0)) %>% summarise(mean(predt == Survived))



# Q4c
cat("\014")
train_set %>% group_by(Sex, Pclass) %>% summarise(prop.table(table(Survived)))


# Q4d
cat("\014")
test_set %>% mutate(predt = ifelse((Sex == "female" & (Pclass == 1 | Pclass == 2)), 1, 0)) %>% summarise(mean(predt == Survived))


# Q5a
cat("\014")
y_hat2 <- ifelse(test_set$Sex =="female", 1, 0) %>% factor(levels = levels(test_set$Survived))
y_hat3 <- ifelse(test_set$Pclass == 1, 1, 0) %>% factor(levels = levels(test_set$Survived))
y_hat4 <- ifelse((test_set$Sex == "female" & (test_set$Pclass == 1 | test_set$Pclass == 2)), 1, 0) %>% factor(levels = levels(test_set$Survived))
cat("\014")
c2 <- confusionMatrix(data=y_hat2,reference=test_set$Survived)
c3 <- confusionMatrix(data=y_hat3,reference=test_set$Survived)
c4 <- confusionMatrix(data=y_hat4,reference=test_set$Survived)
cat("\014")
c2$byClass[c("Sensitivity","Specificity", "Prevalence")]
c3$byClass[c("Sensitivity","Specificity", "Prevalence")]
c4$byClass[c("Sensitivity","Specificity", "Prevalence")]


# Q5a and Q5b
(c2$byClass["Sensitivity"] + c2$byClass["Specificity"])/2
(c3$byClass["Sensitivity"] + c3$byClass["Specificity"])/2
(c4$byClass["Sensitivity"] + c4$byClass["Specificity"])/2


# Q6
cat("\014")
F_meas(data = y_hat2, reference = factor(test_set$Survived))
F_meas(data = y_hat3, reference = factor(test_set$Survived))
F_meas(data = y_hat4, reference = factor(test_set$Survived))
                   
                     





   
  
  





