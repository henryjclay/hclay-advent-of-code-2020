#LIBRARIES####
library(tidyverse)
library(readr)
library(stringr)

#INPUT DATASET####

##Read CSV and sort####
df <- read_csv("puz2_input.txt", col_names = FALSE, col_types = cols(X1 = col_character()))

#PART ONE####

##Parse the data into useful columns####
p1df <- df %>% 
  mutate(policy=str_trim(str_split_fixed(X1, ":", 2)[,1]),password=str_trim(str_split_fixed(X1, ":", 2)[,2])) %>% 
  mutate(letter=str_split_fixed(policy, " ", 2)[,2],min_max=str_split_fixed(policy, " ", 2)[,1]) %>% 
  mutate(min=as.numeric(str_split_fixed(min_max,"-",2)[,1]),max=as.numeric(str_split_fixed(min_max,"-",2)[,2])) %>%
###Count occurrences of the letter in the password####
    mutate(count=as.numeric(str_count(password,letter))) %>% 
###Check against policy####
  mutate(validity = if_else(count<=max & count>=min,"valid","invalid"))

##Count number of valid passwords####
part1_result <- p1df %>%
  count(validity,sort=TRUE)

#PART TWO####

##Parse the data into useful columns####
p2df <- df %>% 
  mutate(policy=str_trim(str_split_fixed(X1, ":", 2)[,1]),password=str_trim(str_split_fixed(X1, ":", 2)[,2])) %>% 
  mutate(letter=str_split_fixed(policy, " ", 2)[,2],pos1_pos2=str_split_fixed(policy, " ", 2)[,1]) %>% 
  mutate(pos1=as.numeric(str_split_fixed(pos1_pos2,"-",2)[,1]),pos2=as.numeric(str_split_fixed(pos1_pos2,"-",2)[,2])) %>%
###do logical check for validity####
  mutate(validity=if_else(xor(substr(password,pos1,pos1) == letter, substr(password,pos2,pos2) == letter), "valid", "invalid"))

##Count number of valid passwords####
part2_result <- p2df %>%
  count(validity,sort=TRUE)

