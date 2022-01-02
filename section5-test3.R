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







