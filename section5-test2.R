library(tidyverse)
library(dslabs)
library(dplyr)
library(caret)
library(ggplot2)
library(rpart)
library(randomForest)


# Q1
cat("\014")
data("tissue_gene_expression")
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
# set.seed(1991)
# fit0 <- caret::train(x, y, method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))
# ggplot(fit0, highlight = TRUE)
# cat("\014")
# fit0$bestTune


# # Q2
# set.seed(1991)
# fit <- caret::train(x, y, method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)), control = rpart.control(minsplit = 0))
# ggplot(fit, highlight = TRUE)
# cat("\014")
# fit$bestTune
# confusionMatrix(fit0)
# confusionMatrix(fit)
# fit_rpart <- fit # for Q6


# # Q3
# cat("\014")
# plot(fit1$finalModel, margin = 0.1)
# text(fit1$finalModel, cex = 0.75)

# # Q4
# set.seed(1991)
# fit <- train(x, y, method = "rf", tuneGrid = data.frame(mtry = seq(50, 200, 25)), nodesize = 1)
# fit$bestTune
# 
# # Q5
# cat("\014")
# varImp(fit)

# # Q6
# cat("\014")
# tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
# cat("\014")
# names <- tree_terms
# VI <- varImp(fit)

df <- data.frame(term = rownames(VI$importance), imp = VI$importance$Overall) %>% mutate(rank = rank(-imp)) %>% arrange(desc(imp)) %>% filter(term == "CFHR4")
df












  

