#LIBRARIES####
library(tidyverse)
library(readr)
library(stringr)
library(tibble)
library(magrittr)
library(tidyr)

#INPUT DATASET####

##Read input####
#test file input: input <- read_file("puz4_test.txt")
input <- read_file("puz4_input.txt")

##Parse Input to Tibble####
df <- as_tibble_col(unlist(str_split(input,"\n\n")),column_name = "passport_raw") %>%  
  mutate(passport_raw=str_replace_all(passport_raw,"\n"," "))
  
#PART ONE####

##Define Policy####
required_fields <- c("byr","iyr","eyr","hgt","hcl","ecl","pid")

##Define check function####
passport_check <- function(passport, policy) {
  str_detect(fixed(passport),policy)
}

##Check Passports####
part1result <- as_tibble_col(df$passport_raw %>%
  map(~ all(passport_check(.x,required_fields))),column_name = "result") %>% count(result)

#PART TWO####

##New check functions!####
###year check####
year_check <- function(data,min,max) {
  if(is_empty(data)) {
    FALSE
    }
  else {
    (str_length(data)==4)&&(as.numeric(data)>=min)&&(as.numeric(data)<=max)
    }
}

###height check####
height_check <- function(data,cm_min,cm_max,in_min,in_max) {
  value <- as.numeric(str_sub(data,end = -3))
  unit <- str_sub(data,-2)
  if(is_empty(data)|isFALSE(str_detect(unit,"[:alpha:]"))) {
    FALSE
    }
  else {
    case_when(
      unit=="cm" ~ (value>=cm_min)&&(value<=cm_max),
      unit=="in" ~ (value>=in_min)&&(value<=in_max),
      )
    }
}

###haircolor check####
haircolor_check <- function(data) {
  string<-str_sub(data,2)
  if(is_empty(data)) {
    FALSE
    } 
  else {
    (str_sub(data,1,1)=="#")&&isFALSE(str_detect(str_sub(string,2),"[f-z]")&&str_detect(str_sub(string,2),"[:punct:]"))
}
}

###eye color policy####
ecl_policy=c("amb","blu","brn","gry","grn","hzl","oth")

###passport ID check####
pid_check <- function(data) {
  if(is_empty(data)) {
    FALSE
    }
  else {
    str_length(data)==9&&!is.na(as.numeric(data))
    }
}

##Improved check function with new policies####
improved_passport_check <- function(passport) {
  ###Temp parse and store passport####
  passport_df <- as_tibble_col(unlist(str_split(passport," ")),column_name = "raw_fields") %>%
    separate(raw_fields, c("field", "data"), sep = ":", remove = FALSE)

  byr <- passport_df$data[which(grepl("byr",passport_df$field))]
  iyr <- passport_df$data[which(grepl("iyr",passport_df$field))]
  eyr <- passport_df$data[which(grepl("eyr",passport_df$field))]
  hgt <- passport_df$data[which(grepl("hgt",passport_df$field))]
  hcl <- passport_df$data[which(grepl("hcl",passport_df$field))]
  ecl <- passport_df$data[which(grepl("ecl",passport_df$field))]
  pid <- passport_df$data[which(grepl("pid",passport_df$field))]
  
  
    ###Check fields####
  byrcheck <- year_check(byr,1920,2002)
  iyrcheck <- year_check(iyr,2010,2020)
  eyrcheck <- year_check(eyr,2020,2030)
  hgtcheck <- height_check(hgt,150,193,59,76)
  hclcheck <- haircolor_check(hcl)
  eclcheck <- any(str_detect(ecl_policy,ecl))
  pidcheck <- pid_check(pid)
  passport_result <- rbind(iyrcheck,byrcheck,eyrcheck,hgtcheck,hclcheck,eclcheck,pidcheck)
  
  all(passport_result)
}

##Screen passports with initial check####
part2result <- cbind(df,as_tibble_col(df$passport_raw %>%
  map(~ all(improved_passport_check(.x))),column_name = "check2result")) %>%
  count(check2result)
  
