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
# input <- "35
# 20
# 15
# 25
# 47
# 40
# 62
# 55
# 65
# 95
# 102
# 117
# 150
# 182
# 127
# 219
# 299
# 277
# 309
# 576"
input <- read_file("puz9_input.txt")

##Parse Input to Tibble####
df <- as_tibble_col(as.numeric(unlist(str_split(input,"\n")),column_name = "value"))

#PART ONE####
##Make Function to check a certain value and it's preamble
puz9_test <- function(test_value,test_preamble) {
  test_preamble <- test_preamble %>% arrange(value)
  ##Save Length of preamble data set####
  len <- dim(test_preamble)[1]
  
  ##set indices
  ti <- 1
  ti1 <- 2
  
  ##Loops####
  while (ti<len) {
    v <- as.numeric(test_preamble[ti,])
    while (ti1<=len) {
      v1 <- as.numeric(test_preamble[ti1,])
      sum <- v + v1
      if (sum == test_value) {
        test_result <- TRUE
        break
      } else if(sum > test_value&(ind<len)) {
        test_preamble <- test_preamble %>% filter(row_number() <= ti1)
      }
      ti1 <- ti1+1
    }
    if (sum == test_value) {
      test_result <- TRUE
      break
    }
    ti <- ti+1
    ti1 <- ti+1
    test_result <- FALSE
  }
  test_result
}

##Now initialize values
part_one_df <- df
preamble_length <- 25

#first index is preamble plus 1
preamble_start <- 1
preamble_end <- preamble_length
#first test values


###prepare values
ind <- preamble_length + 1
len <- dim(part_one_df)[1]

while (ind<len) {
  ####create test variables
  test_preamble_in <- part_one_df %>% slice(preamble_start:preamble_end)
  test_value_in <- as.numeric(part_one_df[ind,1])
  ####test
  test_result <- puz9_test(test_value_in,test_preamble_in)
  if(test_result) {}
  else if(!test_result) {
    found_break <- TRUE
    break_value <- test_value_in
    break
  }
  found_break <- FALSE
  ####update for next loop
  ind <- ind + 1
  preamble_start <- ind - preamble_length
  preamble_end <- ind - 1
}

found_break
break_value

#Part TWO####
##Break Value is the new target
part_two_df <- df

##initialize
ind <- 1
ind1 <- 2
len <- dim(part_one_df)[1]

while (ind<len) {
  ##main loop
  ind1 <- ind + 1
  value <- as.numeric(part_two_df[ind,1])
  test_list <- c(value)
  test_sum <- sum(test_list)
  
  ##sub loop
  while (test_sum<break_value) {
    value <- as.numeric(part_two_df[ind1,1])
    test_list <- append(test_list,value)
    test_sum <- sum(test_list)
    if (test_sum==break_value) {
      break
    }
    ind1 <- ind1 + 1
  }
  if (test_sum==break_value) {
    break
  }
  ind <- ind + 1
}

#Answer!!
part_two_answer <- min(test_list) + max(test_list)
part_two_answer
