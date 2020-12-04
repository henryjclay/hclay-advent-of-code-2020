#LIBRARIES####
library(tidyverse)
library(readr)
library(stringr)
library(tibble)
library(magrittr)

#INPUT DATASET####

##Read input####
#test file input: input <- read_file("puz4_test.txt")
input <- read_file("puz4_input.txt")

##Parse Input to Tibble####
df <- as_tibble_col(unlist(str_split(input,"\n\n")),column_name = "passport_raw") %>%  
  mutate(passport_raw=str_replace_all(passport_raw,"\n"," "))
  
#PART ONE####

##Define Policy####
# policy <- tribble(
#   ~required_fields,
#   "byr",
#   "iyr",
#   "eyr",
#   "hgt",
#   "hcl",
#   "ecl",
#   "pid"
#   )
required_fields <- c("byr","iyr","eyr","hgt","hcl","ecl","pid")

##Define check function####
passport_check <- function(passport, policy) {
  str_detect(fixed(passport),policy)
}

##Check Passports####
part1result <- as_tibble_col(df$passport_raw %>%
  map(~ all(passport_check(.x,required_fields))),column_name = "result") %>% count(result)

#PART TWO####
##WIPWIPWIPWIP####