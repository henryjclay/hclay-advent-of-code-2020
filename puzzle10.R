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
# input <- "28
# 33
# 18
# 42
# 31
# 14
# 46
# 20
# 48
# 47
# 24
# 23
# 49
# 45
# 19
# 38
# 39
# 11
# 1
# 32
# 25
# 35
# 8
# 17
# 7
# 9
# 4
# 2
# 34
# 10
# 3"
input <- read_file("puz10_input.txt")

##Parse Input to Tibble####
df <- as_tibble_col(as.numeric(unlist(str_split(input,"\n")),column_name = "joltage"))

#PART ONE####
device_joltage <- max(df) + 3
df <- bind_rows(df,tibble("value"=device_joltage))
adapters <- as.numeric(dim(df)[1])
ind <- 1
adapter_joltages <- c(1,2,3)
joltage_table <- tibble("value"=0,"joltage_delta"=0)
current_joltage <- as.numeric(joltage_table[ind,1])


while (ind<=adapters) {
  for(val in adapter_joltages) {
    next_joltage <- df %>% filter(value==(current_joltage + val))
    result <- as.numeric(dim(next_joltage)[1])
    if(result==1) {
      break
    }
  }
  next_joltage <- next_joltage %>% mutate("joltage_delta"=(value-current_joltage))
  joltage_table <- bind_rows(joltage_table,next_joltage)
  
  ind <- ind + 1
  current_joltage <- as.numeric(joltage_table[ind,1])
}

counts <- joltage_table %>% filter(joltage_delta==1|joltage_delta==3) %>% count(joltage_delta)
count_one <- counts %>% filter(joltage_delta==1) %>% pull(n)
count_three <- counts %>% filter(joltage_delta==3) %>% pull(n)

count_one * count_three

#PART_TWO####