#LIBRARIES####
library(tidyverse)
library(readr)
library(stringr)
library(tibble)
library(magrittr)
library(tidyr)

#INPUT DATASET####

##Read input####
#test input: input <- "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags."
input <- read_file("puz7_input.txt")

#PART ONE####

##Parse Input to Tibble####
df <- as_tibble_col(unlist(str_split(input,"\n")),column_name = "raw") %>% separate(raw, c("primary", "dependents_raw"), sep = " bags contain ", remove = FALSE) %>% mutate(dependents_listed=str_split(dependents_raw,", "))

##Bag Search Function####
#Finds any parents given a list of bags
bag_search <- function(dependent_list, bag_list) {
  any(str_detect(dependent_list,bag_list))
}
#new function to search for primary bags from list
primary_bag_search <- function(primary_list, bag_list) {
  any(str_detect(primary_list,bag_list))
}

##Find Initial search to Start####
#initial bags are ones that contain a shiny gold bag
answer <- 0
target_bag <- "shiny gold"
search_list <- df %>% filter(grepl(target_bag,dependents_raw))
#add initial results to answer
answer <- as_tibble_col(search_list$primary,column_name = "bags")

##Run loop to search through all parents####
while (!is_empty(search_list$primary)) {
  ##search for parents of the search list
  search_result <- df %>% mutate(search = map_chr(dependents_raw,bag_search,search_list$primary)) %>% filter(search==TRUE)
  ##update the search list for next loop
  search_list <- search_result
  ##record results to answer
  answer <- rbind(answer,as_tibble_col(search_list$primary,column_name = "bags"))
}

##the answer####
nrow(distinct(answer))

#PART TWO####
#create data frame of all dependents of target bag
search_list <- df %>%  filter(grepl(target_bag,primary))
output <- search_list

##Run loop to search through all dependents####
while (!all(grepl("no other",search_list$dependents_raw))) {
  ##search for parents of the search list
  search_result <- df %>% mutate(search = map_chr(dependents_raw,bag_search,search_list$primary)) %>% filter(search==TRUE)
  ##update the search list for next loop
  search_list <- search_result
  ##record results to answer
  answer <- rbind(answer,as_tibble_col(search_list$primary,column_name = "bags"))
}

part2 <- distinct(answer)
search_result <- df %>% mutate(search = map_chr(primary,primary_bag_search,part2$bags)) %>% filter(search==TRUE)
search_list <- search_result %>% filter(grepl("no other bags",dependents_raw))

extract_dependents <- function(dependents_listed) {
  dependents <- unlist(dependents_listed)
  numbers <- str_extract(dependents,"[:digit:]")
  colors <- str_match(dependents,"[:digit:] (.*) bag")
  mapply(c, numbers, colors[,2], SIMPLIFY = TRUE)
}

df <- df %>% mutate(dependents = map(dependents_listed,extract_dependents))

