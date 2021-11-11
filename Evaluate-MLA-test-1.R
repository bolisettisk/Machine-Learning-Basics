library(caret)
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggrepel)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type


#Q1
cat("\014")
dat %>% group_by(type) %>% summarise(mean(sex == "Female"))

#Q2
cat("\014")

y_hat <- ifelse(dat$type == "inclass", "Female", "Male") %>% factor(levels = c("Female", "Male"))
mean(y_hat == dat$sex)
# or
y_hat <- ifelse(x == "inclass", "Female", "Male") %>% factor(levels = levels(y))
mean(y_hat == dat$sex)


#Q3
cat("\014")
table(y_hat, y)

#Q4
cat("\014")
sensitivity(y_hat, y)

#Q5
cat("\014")
specificity(y_hat, y)

#Q6
cat("\014")
prev <- dat %>% summarise(mean(sex == "Female"))
prev
