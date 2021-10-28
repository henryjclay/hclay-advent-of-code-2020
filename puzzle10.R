#LIBRARIES####
library(tidyverse)
library(readr)
library(stringr)
library(tibble)
library(magrittr)
library(tidyr)

#INPUT DATASET####

##Read input####
#test input:
# input <- "16
# 10
# 15
# 5
# 1
# 11
# 7
# 19
# 6
# 12
# 4"
input <- read_file("puz10_input.txt")

##Parse Input to Tibble####
df <- as_tibble_col(as.numeric(unlist(str_split(input,"\n")),column_name = "value"))
