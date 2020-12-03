#LIBRARIES####
library(tidyverse)
library(readr)

#INPUT DATASET####

##Read CSV and sort####
df <- read_csv("puz1_input.csv") %>% arrange(number)
df1 <- df


#PART 1####

##Save Length of data set####
len <- dim(df)[1]
ind <- 1
ind1 <- 2

##Loops####
while (ind<len) {
  v <- df[ind,]
  while (ind1<len) {
    v1 <- df[ind1,]
    sum <- v + v1
    product1 <- v*v1
    if (sum == 2020) {
      break
      } else if (sum>2020) {
      df <- df %>% filter(row_number() <= ind1)
      len <- dim(df)[1]
      break
      }
    ind1 <- ind1+1
    }
  if (sum == 2020) {
    paste("Sum: ",sum,"   Product: ",product1)
    break
    }
  ind <- ind+1
  ind1 <- ind+1
}

##Print Answer####
paste("The answer to Part 1 is:   ","  N1:",v,"  N2:",v1,"  Sum:",sum,"  Product:",product1)


#PART 2####

##Re-initialize
len <- dim(df1)[1]
ind <- 1
ind1 <- 2
ind2 <- 3

##Loops####
while (ind<len) {
  v <- df1[ind,]
    while (ind1<len) {
    v1 <- df1[ind1,]
  
    while (ind2<=len) {
      v2 <- df1[ind2,]
      sum <- v + v1 + v2
      product2 <- v*v1*v2
      if (sum == 2020) {
        break
      } else if ((sum>2020)&&(ind==1)&&(ind1==2)) {
        df1 <- df1 %>% filter(row_number() < ind2) #%>% filter(row_number() != ind)
        len <- dim(df1)[1]
        break
      }
      ind2 <- ind2+1
    }
    if (sum == 2020) {break}
    ind1 <- ind1+1
    ind2 <- ind1+1
    }
  if (sum == 2020) {break}
  ind <- ind+1
  ind1 <- ind+1
}

##Print Answer####
paste("The answer to Part 2 is:   ","  N1:",v,"  N2:",v1,"  N3:",v2,"  Sum:",sum,"  Product:",product2)


#HOW I DID PART 2 THE FIRST TIME####
# for (val1 in 1:dim(df)[1]) {
#   n1=df[val1,]
#   for (val2 in 1:dim(df)[1]) {
#     n2=df[val2,]
#     for (val3 in 1:dim(df)[1]) {
#       n3=df[val3,]
#       if((n1 + n2 + n3) == 2020) {
#         result = n1*n2*n3
#         rn1=n1
#         rn2=n2
#         rn3=n3
#         break
#       }
#     }
#   }
# }

