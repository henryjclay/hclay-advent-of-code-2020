#LIBRARIES####
library(tidyverse)
library(readr)
library(stringr)

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
slope1 <- c(1,1)
slope2 <- c(3,1)
slope3 <- c(5,1)
slope4 <- c(7,1)
slope5 <- c(1,2)
slopes <- rbind(slope1,slope2,slope3,slope4,slope5)

##Define map extents####
map_width <- length(map[1,])
map_height <- length(map[,1])
map_result <- matrix(nrow=map_height,ncol=map_width)
map_result[] <- 0L

for (val in 1:length(slopes[,1])) {
##Define movement####
hmove <- as.numeric(slopes[val,1])
vmove <- as.numeric(slopes[val,2])

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
part2results <- rbind(part2results,c("Slope"=slope,"Number of Trees"=result))

###reset vars####
h <- 1
v <- 1
map_result[] <- 0L
}


