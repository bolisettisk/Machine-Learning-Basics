library(tidyverse)
library(dslabs)
library(dplyr)
library(caret)
library(ggplot2)
library(rpart)
library(matrixStats)
library(randomForest)
library(Rborist)
library(ggrepel)


data("movielens")

# cat("\014")
# head(movielens)
# 
# cat("\014")
# movielens %>%
#   summarize(n_users = n_distinct(userId),
#             n_movies = n_distinct(movieId))
# 
# keep <- movielens %>%
#   dplyr::count(movieId) %>%
#   top_n(5) %>%
#   pull(movieId)
# 
# tab <- movielens %>%
#   filter(userId %in% c(13:20)) %>%
#   filter(movieId %in% keep) %>%
#   select(userId, title, rating) %>%
#   spread(title, rating)
# tab %>% knitr::kable()
# 
# cat("\014")
# users <- sample(unique(movielens$userId), 100)
# rafalib::mypar()
# movielens %>% filter(userId %in% users) %>%
#   select(userId, movieId, rating) %>%
#   mutate(rating = 1) %>%
#   spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>%
#   as.matrix() %>% t(.) %>%
#   image(1:100, 1:100,. , xlab="Movies", ylab="Users")
# abline(h=0:100+0.5, v=0:100+0.5, col = "grey")
# 
# movielens %>%
#   dplyr::count(movieId) %>%
#   ggplot(aes(n)) +
#   geom_histogram(bins = 30, color = "black") +
#   scale_x_log10() +
#   ggtitle("Movies")
# 
# movielens %>%
#   dplyr::count(userId) %>%
#   ggplot(aes(n)) +
#   geom_histogram(bins = 30, color = "black") +
#   scale_x_log10() +
#   ggtitle("Users")
# 
# 
# cat("\014")
# set.seed(755)
# test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, list = FALSE)
# train_set <- movielens[-test_index,]
# test_set <- movielens[test_index,]
# test_set <- test_set %>% semi_join(train_set, by = "movieId") %>% semi_join(train_set, by = "userId")
# 
# RMSE <- function(true_ratings, predicted_ratings){
#   sqrt(mean((true_ratings - predicted_ratings)^2))
# }
# 
# 
# cat("\014")
# mu_hat <- mean(train_set$rating)
# # mu_hat
# naive_rmse <- RMSE(test_set$rating, mu_hat)
# # naive_rmse
# # predictions <- rep(2.5, nrow(test_set)) # checking for a value other than mu_hat
# # RMSE(test_set$rating, predictions)
# 
# rmse_results <- data_frame(method = "just the average", RMSE = naive_rmse)
# 
# 
# # Modelling movie effect
# cat("\014")
# # fit <- lm(rating ~ as.factor(movieId), data = movielens)
# mu <- mean(train_set$rating)
# movie_avgs <- train_set %>% group_by(movieId) %>% summarise(b_i = mean(rating - mu))
# movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))
# predicted_ratings <- mu + test_set %>% left_join(movie_avgs, by="movieId") %>% .$b_i
# model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
# 
# cat("\014")
# rmse_results <- bind_rows(rmse_results, data_frame(method = "Movie Effect Model", RMSE = model_1_rmse))
# rmse_results %>% knitr::kable()
# 
# # Modelling user effect
# train_set %>% group_by(userId) %>% summarise(b_u = mean(rating)) %>% filter(n() >= 100) %>% ggplot(aes(b_u)) + geom_histogram(bins = 30, color = "black")
# 
# # lm(rating ~ as.factor(movieId) + as.factor(userId))
# user_avgs <- train_set %>% left_join(movie_avgs, by = "movieId") %>% group_by(userId) %>% summarise(b_u = mean(rating - mu - b_i))
# 
# predicted_ratings <- test_set %>%
#   left_join(movie_avgs, by='movieId') %>%
#   left_join(user_avgs, by='userId') %>%
#   mutate(pred = mu + b_i + b_u) %>%
#   .$pred
# 
# model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
# rmse_results <- bind_rows(rmse_results,
#                           data_frame(method="Movie + User Effects Model",
#                                      RMSE = model_2_rmse ))
# cat("\014")
# rmse_results %>% knitr::kable()
# 
# 
# ##### Regularization
# cat("\014")
# test_set %>%
#   left_join(movie_avgs, by='movieId') %>%
#   mutate(residual = rating - (mu + b_i)) %>%
#   arrange(desc(abs(residual))) %>%
#   select(title,  residual) %>% slice(1:10) %>% knitr::kable()
# 
# movie_titles <- movielens %>%
#   select(movieId, title) %>%
#   distinct()
# 
# movie_avgs %>% left_join(movie_titles, by="movieId") %>%
#   arrange(desc(b_i)) %>%
#   select(title, b_i) %>%
#   slice(1:10) %>%
#   knitr::kable()
# 
# movie_avgs %>% left_join(movie_titles, by="movieId") %>%
#   arrange(b_i) %>%
#   select(title, b_i) %>%
#   slice(1:10) %>%
#   knitr::kable()
# 
# train_set %>% dplyr::count(movieId) %>%
#   left_join(movie_avgs) %>%
#   left_join(movie_titles, by="movieId") %>%
#   arrange(desc(b_i)) %>%
#   select(title, b_i, n) %>%
#   slice(1:10) %>%
#   knitr::kable()
# 
# train_set %>% dplyr::count(movieId) %>%
#   left_join(movie_avgs) %>%
#   left_join(movie_titles, by="movieId") %>%
#   arrange(b_i) %>%
#   select(title, b_i, n) %>%
#   slice(1:10) %>%
#   knitr::kable()
# 
# cat("\014")
# lambda <- 3
# mu <- mean(train_set$rating)
# movie_reg_avgs <- train_set %>%
#   group_by(movieId) %>%
#   summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())
# 
# 
# data_frame(original = movie_avgs$b_i,
#            regularlized = movie_reg_avgs$b_i,
#            n = movie_reg_avgs$n_i) %>%
#   ggplot(aes(original, regularlized, size=sqrt(n))) +
#   geom_point(shape=1, alpha=0.5)
# 
# cat("\014")
# train_set %>%
#   dplyr::count(movieId) %>%
#   left_join(movie_reg_avgs) %>%
#   left_join(movie_titles, by="movieId") %>%
#   arrange(desc(b_i)) %>%
#   select(title, b_i, n) %>%
#   slice(1:10) %>%
#   knitr::kable()
# 
# train_set %>%
#   dplyr::count(movieId) %>%
#   left_join(movie_reg_avgs) %>%
#   left_join(movie_titles, by="movieId") %>%
#   arrange(b_i) %>%
#   select(title, b_i, n) %>%
#   slice(1:10) %>%
#   knitr::kable()
# 
# predicted_ratings <- test_set %>%
#   left_join(movie_reg_avgs, by='movieId') %>%
#   mutate(pred = mu + b_i) %>%
#   .$pred
# 
# model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
# rmse_results <- bind_rows(rmse_results,
#                           data_frame(method="Regularized Movie Effect Model",
#                                      RMSE = model_3_rmse ))
# rmse_results
# 
# 
# cat("\014")
# lambdas <- seq(0, 10, 0.25)
# mu <- mean(train_set$rating)
# just_the_sum <- train_set %>%
#   group_by(movieId) %>%
#   summarize(s = sum(rating - mu), n_i = n())
# rmses <- sapply(lambdas, function(l){
#   predicted_ratings <- test_set %>%
#     left_join(just_the_sum, by='movieId') %>%
#     mutate(b_i = s/(n_i+l)) %>%
#     mutate(pred = mu + b_i) %>%
#     .$pred
#   return(RMSE(predicted_ratings, test_set$rating))
# })
# qplot(lambdas, rmses)
# lambdas[which.min(rmses)]
# 
# lambdas <- seq(0, 10, 0.25)
# rmses <- sapply(lambdas, function(l){
#   mu <- mean(train_set$rating)
#   b_i <- train_set %>%
#     group_by(movieId) %>%
#     summarize(b_i = sum(rating - mu)/(n()+l))
#   b_u <- train_set %>%
#     left_join(b_i, by="movieId") %>%
#     group_by(userId) %>%
#     summarize(b_u = sum(rating - b_i - mu)/(n()+l))
#   predicted_ratings <-
#     test_set %>%
#     left_join(b_i, by = "movieId") %>%
#     left_join(b_u, by = "userId") %>%
#     mutate(pred = mu + b_i + b_u) %>%
#     .$pred
#   return(RMSE(predicted_ratings, test_set$rating))
# })
# 
# qplot(lambdas, rmses)
# 
# lambda <- lambdas[which.min(rmses)]
# lambda
# 
# cat("\014")
# rmse_results <- bind_rows(rmse_results,
#                           data_frame(method="Regularized Movie + User Effect Model",
#                                      RMSE = min(rmses)))
# rmse_results %>% knitr::kable()


#### Matrix Factorization
train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% #3252 is Scent of a Woman used in example
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()
rownames(y)<- y[,1]
y <- y[,-1]
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))

m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
# qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
# qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

m_4 <- "You've Got Mail"
m_5 <- "Sleepless in Seattle"
# qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

# cor(y[, c(m_1, m_2, m_3, m_4, m_5)], use="pairwise.complete") %>% 
#   knitr::kable()


set.seed(1)
options(digits = 2)
Q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
rownames(P) <- 1:nrow(P)

X <- jitter(P%*%t(Q))
X %>% knitr::kable(align = "c")
cor(X)

cat("\014")
t(Q) %>% knitr::kable(aling="c")
P

set.seed(1)
options(digits = 2)
m_6 <- "Scent of a Woman"
Q <- cbind(c(1 , 1, 1, -1, -1, -1), 
           c(1 , 1, -1, -1, -1, 1))
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
P <- cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(P) <- 1:nrow(X)

X <- jitter(P%*%t(Q), factor=1)
X %>% knitr::kable(align = "c")

cor(X)

cat("\014")
t(Q) %>% knitr::kable(align="c")

P

# six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
# tmp <- y[,six_movies]
# cor(tmp, use="pairwise.complete")


###### SVD and PCA
y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

cat("\014")
dim(pca$rotation)
dim(pca$x)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)


pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))

cat("\014")
pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)

pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)

