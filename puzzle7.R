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

##Parse Input to Tibble####
df <- as_tibble_col(unlist(str_split(input,"\n")),column_name = "raw") %>% separate(raw, c("primary", "dependents_raw"), sep = " bags contain ", remove = FALSE) %>% mutate(dependents_listed=str_split(dependents_raw,", "))


#PART ONE####

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

##The Answer####
nrow(distinct(answer))

#PART TWO####
# Part 2 test input: input <- "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags."

#create data frame of all dependents of target bag as inital search list
target_bag <- "shiny gold"
search_list <- df %>%  filter(grepl(target_bag,primary))
# add this to your answer list of bags to start
part_two_out <- as_tibble_col(search_list$primary, column_name = "primary")

#### Extract dependents function takes list of dependents and extracts/flattens into list of all dependent bags (including duplicates)####
extract_dependents <- function(dependents_listed) {
  dependents_structured <- tibble(raw = unlist(dependents_listed)) %>% mutate(qty = as.numeric(str_extract(raw,"[:digit:]")), color = str_match(raw,"[:digit:] (.*) bag")[,2])
  dependents_structured <- dependents_structured %>% rowwise() %>% mutate(full_list = list(rep(color,qty)))
  output <- as_tibble_col(unlist(dependents_structured$full_list), column_name = "primary")
  output
}

##Run loop to search through all parents####
while (!is_empty(search_list$primary)) {
# first extract all the dependents of the search list results using the function
temp_extraction <- extract_dependents(search_list$dependents_listed)
# then add extraction to output list
part_two_out <- rbind(part_two_out,temp_extraction)
# next build new search list from extraction
search_list <- left_join(temp_extraction,df,by = "primary")
# finally remove any bags that have no dependents
search_list <- search_list %>% filter(!grepl("no other bags",dependents_raw))
}

##The Answer####
#remove target bag
part_two_out <- part_two_out %>%  filter(!grepl(target_bag,primary))
nrow(part_two_out)

