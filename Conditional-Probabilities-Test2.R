library(caret)
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggrepel)
library(lubridate)


data("heights")
# MISSING CODE
heights %>% mutate(height = round(height)) %>% group_by(height) %>% summarize(p = mean(sex == "Male")) %>% qplot(height, p, data =.)
#
