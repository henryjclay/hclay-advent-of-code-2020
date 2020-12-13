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
input <- "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
input <- read_file("puz8_input.txt")

#PART ONE####

##Parse Input to Tibble####
df <- as_tibble_col(unlist(str_split(input,"\n")),column_name = "raw") %>% separate(raw, c("operation", "value"), sep = " ", remove = FALSE) %>% mutate(run_count=0)

##initialize some variables####
accum <- 0
i <- 1

# acc <- function(value, i) {
#   accum <- accum + as.numeric(value)
#   i <- i + 1
#   c(accum,i)
# }
# 
# jmp <- function(value,i) {
#   i <- i + as.numeric(value)
#   c(accum,i) 
# }
# 
# nop <- function(value,i) {
#   i <- i + 1
#   c(accum,i)
# }

##run loop until you hit a second run_count####
while (!any(df$run_count==2)) {
  value <- as.numeric(df[i,3])
  if(df[i,2]=="acc") {
    accum <- accum + as.numeric(value)
    i <- i + 1
  } else if(df[i,2]=="jmp") {
    i <- i + value
  } else if(df[i,2]=="nop") {
    i <- i + 1
  }
  df[i,4] <- df[i,4] + 1
}

accum

#PART TWO####

