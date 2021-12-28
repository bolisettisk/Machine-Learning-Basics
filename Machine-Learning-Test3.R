library(dslabs)
library(tidyverse)
library(caret)
library(dplyr)
library(ggplot2)


# Q3.1.1.1
cat("\014")
data("tissue_gene_expression")

set.seed(1993) #if using R 3.5 or earlier
# set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
fit <- train(y ~ ., method = "lda", data = data.frame(x = x, y = y))
fit$results
# or
fit1 <- train(x, y, method = "lda")
fit1$results


# Q3.1.1.2
cat("\014")
genes <- colnames(fit1$finalModel$means, do.NULL = TRUE, prefix = "col")
df <- data.frame(genes = genes, cerebellum = as.numeric(fit1$finalModel$means[1,]), hippocampus = as.numeric(fit1$finalModel$means[2,]))
df %>% ggplot(aes(cerebellum, hippocampus, colour = genes)) + geom_point() + geom_text(label = genes)


# Q3.1.1.3
cat("\014")
set.seed(1993) #set.seed(1993, sample.kind="Rounding") if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
fit <- train(x, y, method = "qda")
fit$results


# Q3.1.1.4
cat("\014")
genes <- colnames(fit$finalModel$means, do.NULL = TRUE, prefix = "col")
df <- data.frame(genes = genes, cerebellum = as.numeric(fit$finalModel$means[1,]), hippocampus = as.numeric(fit$finalModel$means[2,]))
df_reshape <- df %>% pivot_longer(c("cerebellum", "hippocampus"), names_to = "Tissue", values_to = "Value")
df_reshape %>% ggplot(aes(genes, Value, colour = Tissue)) + geom_point(label = genes)


# Q3.1.1.5
cat("\014")
fit <- train(x, y, method = "lda", preProcess = "center")
fit$results
genes <- colnames(fit$finalModel$means, do.NULL = TRUE, prefix = "col")
df <- data.frame(genes = genes, cerebellum = as.numeric(fit$finalModel$means[1,]), hippocampus = as.numeric(fit$finalModel$means[2,]))
df_reshape <- df %>% pivot_longer(c("cerebellum", "hippocampus"), names_to = "Tissue", values_to = "Value")
df_reshape %>% group_by("Tissue") %>% ggplot(aes(genes, Value, colour = Tissue, label = Tissue)) + geom_point() #+ geom_text(aes(x = Value, lable = genes), show.legend = FALSE)


# Q3.1.1.6
cat("\014")
# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
fit <- train(x, y, method = "lda", preProcess = "center")
fit$results

