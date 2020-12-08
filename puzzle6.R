#LIBRARIES####
library(tidyverse)
library(readr)
library(stringr)
library(tibble)
library(magrittr)
library(tidyr)

#INPUT DATASET####

##Read input####
#test input: input <- "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"
input <- read_file("puz6_input.txt")

#PART ONE####

##Parse Input to Tibble####
df <- as_tibble_col(unlist(str_split(input,"\n\n")),column_name = "group_raw") %>%  
  mutate(group=str_squish(group_raw)) %>% 
  mutate(group=str_replace_all(group," ",""))

##Check unique answers and sum####    
answer <- unlist(df$group %>% map(~ sum(!!str_count(.x,letters))))

sum(answer)

#PART TWO####

##Parse Input to Tibble, but this time need to actually parse each group to a list####
df <- as_tibble_col(unlist(str_split(input,"\n\n")),column_name = "group_raw") %>%  
  mutate(group=str_split(group_raw,"\n"))

check_group <- function(group) {
  counts <- lapply(unlist(group),str_count,letters)
  sumcounts <- colSums(matrix(unlist(counts),ncol = 26,byrow = TRUE))
  length(which(length(unlist(group))==sumcounts))
}

answerlist <- df$group %>% map(~ check_group(.x))

sum(unlist(answerlist))