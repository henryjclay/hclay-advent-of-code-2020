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
# input <- "nop +0
# acc +1
# jmp +4
# acc +3
# jmp -3
# acc -99
# acc +1
# jmp -4
# acc +6"
input <- read_file("puz8_input.txt")

#PART ONE####

##Parse Input to Tibble####
df <- as_tibble_col(unlist(str_split(input,"\n")),column_name = "raw") %>% separate(raw, c("operation", "value"), sep = " ", remove = FALSE) %>% mutate(run_count=0)

##initialize some variables####
accum <- 0
i <- 1

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

##make part one into a function
test_program <- function(program) {
  ### initialize
  accum <- 0
  i <- 1
  end <- dim(program)[1]+1
  test_df <- program
  
  ### loop until second run count is detected
  while ((!any(test_df$run_count==2))&(i<end)) {
    value <- as.numeric(test_df[i,3])
    if(test_df[i,2]=="acc") {
      accum <- accum + as.numeric(value)
      i <- i + 1
    } else if(test_df[i,2]=="jmp") {
      i <- i + value
    } else if(test_df[i,2]=="nop") {
      i <- i + 1
    }
    test_df[i,4] <- test_df[i,4] + 1
  }
  test_df
  if(i==end) {
    result <- TRUE
  } else {
    result <- FALSE
  }
  c(result,accum)
}

##initialize some variables####
accum <- 0
i <- 1
test_result <- test_program(df)
test_logic_result <- as.logical(test_result[1])
test_accum_result <- test_result[2]

##run loop until you are able to get program to pass the test####
while (!test_logic_result) {
  test_df <- df
  value <- as.numeric(df[i,3])
  if(df[i,2]=="acc") {
    #if acc run normally
    accum <- accum + as.numeric(value)
    i <- i + 1
  } else if(df[i,2]=="jmp") {
    # if jmp test changing to nop
    test_df[i,2] <- "nop"
    ##test the program
    test_result <- test_program(test_df)
    test_logic_result <- as.logical(test_result[1])
    test_accum_result <- test_result[2]
    if(test_logic_result) {
      ### if it passes break the loop
      break
    } else if(!test_logic_result) {
      ### if it fails do the original operation
    i <- i + value
    }
    
  } else if(df[i,2]=="nop") {
    # if nop test changing to jmp
    test_df[i,2] <- "jmp"
    ##test the program
    test_result <- test_program(test_df)
    test_logic_result <- as.logical(test_result[1])
    test_accum_result <- test_result[2]
    if(test_logic_result) {
      ### if it passes break the loop
      break
    } else if(!test_logic_result) {
      ### if it fails do the original operatio
      i <- i + 1
    }
  }
  #df[i,4] <- df[i,4] + 1
}

##ANSWER
test_accum_result
