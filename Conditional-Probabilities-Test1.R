library(caret)
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggrepel)
library(lubridate)


#Q1

P_pd = 0.85 # Pr(test+|Disease)
P_nh = 0.90 # Pr(test-|Healthy)
P_d = 0.02 # Pr(Disease)

# Pr(Disease|test+) = Pr(test+|Disease)*(Pr(Disease)/Pr(test+))
#Pr(test+) = Pr(test+|Disease)*Pr(Disease) + Pr(test+|Healthy)*Pr(Healthy)
#          = Pr(test+|Disease)*Pr(Disease) + (1 - Pr(test-|Healthy))*(1-Pr(Disease))

cat("\014")
P_dp <- P_pd*P_d/((P_pd*P_d)+((1-P_nh)*(1-P_d)))
P_dp


#Q2
P_pd = 0.85 # Pr(test+|Disease)
P_nh = 0.90 # Pr(test-|Healthy)
P_d = 0.02 # Pr(Disease)

set.seed(1) # if using R 3.5 or earlier
# set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

P_p = mean(test) # Pr(Positive)
P_p


#Q3
cat("\014")
P_ds <- mean(disease)
P_ns <- 1 - mean(test)
P_nds <- 1 - mean(test[disease == 1])
P_nds*P_ds/P_ns


#Q4
cat("\014")
P_ds <- mean(disease)
P_ps <- mean(test)
P_pds <- mean(test[disease == 1])
P_pds*P_ds/P_ps


#Q5
cat("\014")
P_dps <- P_pds*P_ds/P_ps
P_dps/P_ds





