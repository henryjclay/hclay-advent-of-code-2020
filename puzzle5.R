#LIBRARIES####
library(tidyverse)
library(readr)
library(stringr)
library(tibble)
library(magrittr)
library(tidyr)

#INPUT DATASET####

##Read input####
#test file input: input <- read_file("puz5_test.txt")
input <- read_file("puz5_input.txt")

##Parse Input to Tibble####
df <- as_tibble_col(unlist(str_split(input,"\n")),column_name = "boarding_passes")

#PART ONE####

##Functions####
check_row <- function(boarding_pass,rows) {
  for(val in 1:7) {
    node <- str_sub(boarding_pass,val,val)
    split <- length(rows)/2
    rows <- case_when(
      node == "F" ~ rows[1:(split)],
      node == "B" ~ rows[(split+1):length(rows)]
    )
  }
  rows
}

check_column <- function(boarding_pass,columns) {
  for(val in 8:10) {
    node <- str_sub(boarding_pass,val,val)
    split <- length(columns)/2
    columns <- case_when(
      node == "L" ~ columns[1:(split)],
      node == "R" ~ columns[(split+1):length(columns)]
    )
  }
  columns
}

check_boarding_pass <- function(boarding_pass,rows,columns) {
  row <- check_row(boarding_pass,rows)
  column <- check_column(boarding_pass,columns)
  seatID <- (row*8)+column
  seatID
}
  
##Define the plane####
plane_rows <- 0:127
plane_columns <- 0:7

##Check Boarding Passes####
result <- as_tibble(df) %>% cbind(
  seatID=unlist(
    as.numeric(df$boarding_passes %>% 
      map(~ check_boarding_pass(.x,plane_rows,plane_columns))
    ))
  ) %>% arrange(seatID)

##get Part one answer####
maxseatID <- max(result$seatID)

#PART TWO####

##Initialize some vars####
seatfound <- FALSE
val <- 1

##search for seat####
while (seatfound!=TRUE) {
  seatfound <- ifelse((result[val,2]+1==result[val+1,2]),FALSE,TRUE)
  val <- val+1
}

myseat <- result[val,2]-1