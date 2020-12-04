#LIBRARIES####
library(tidyverse)
library(readr)
library(stringr)
library(tibble)

#INPUT DATASET####

##Read input####

df <- read_csv("puz3_input.txt", col_names = FALSE, col_types = cols(X1 = col_character())) %>% mutate(lists = str_split(X1,""))
#for testing --> df <- read_csv("puz3_test.txt", col_names = FALSE, col_types = cols(X1 = col_character())) %>% mutate(lists = str_split(X1,""))

row_width <- str_length(df[1,1])

map <- matrix(unlist(df[,2]),ncol=row_width,byrow=TRUE)

#PART ONE####

##Define starting locations####
h <- 1
v <- 1

##Define movement####
hmove <- 3
vmove <- 1

##Define map extents####
map_width <- length(map[1,])
map_height <- length(map[,1])
map_result <- matrix(nrow=map_height,ncol=map_width)
map_result[] <- 0L

##Loop to move!####

while (v<=map_height) {
  ###check location for tree
  map_result[v,h] <- case_when(
    map[v,h] == "#"~1,
    map[v,h] == "."~0)
  ###move####
  h <- ((h-1) + hmove)%%map_width + 1
  v <- v + vmove
}

part1results <- map_result %>% sum()

#PART TWO####

##Define starting locations####
h <- 1
v <- 1

##Define Slope Sets to Test####
part2 <- tribble(
  ~slope, ~v, ~h,~trees,
  "slope1", 1, 1,0,
  "slope2", 3, 1,0,
  "slope3", 5, 1,0,
  "slope4", 7, 1,0,
  "slope5", 1, 2,0,
)

##Define map extents####
map_width <- length(map[1,])
map_height <- length(map[,1])
map_result <- matrix(nrow=map_height,ncol=map_width)
map_result[] <- 0L

for (val in 1:length(slopes[,1])) {
  ##Define movement####
  hmove <- as.numeric(part2[val,2])
  vmove <- as.numeric(part2[val,3])

  ##Loop to move!####

  while (v<=map_height) {
    ###check location for tree
    map_result[v,h] <- case_when(
      map[v,h] == "#"~1,
      map[v,h] == "."~0)
    ###move####
    h <- ((h-1) + hmove)%%map_width + 1
    v <- v + vmove
  }

  ###store result####
  slope <- paste("slope",val)
  result <- map_result %>% sum()
  part2[val,4] <- result

  ###reset vars####
  h <- 1
  v <- 1
  map_result[] <- 0L
}

part2answer <- prod(part2[,4])

