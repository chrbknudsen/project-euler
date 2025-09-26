# library(tidyverse)
# 
# Nå. hvilke i de ydre ringe skærmes af punkter længere inde?
#   i den første ring, har vi 6 punkter.
# I den anden har vi 12
# 
# n = 5 giver 6 punkter på hver kant.
# 
# n= 1 - 6 punkter 
# n = 2 12
# n=3, 18
# Antal punkter på vejen rundt: 6*n
# 
# Og position kan derfor også betegnes som:
#   x/6n, x fra 1 til 6n
# 
# for n = 1, får vi 1/6, 2/6, 3/6, 4/6, 5/6 og 6/6
# I n = 2, får vi 1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12, 10/12, 11/12 og 12/12
# 
# Eller, reduseret; hhv
# 1/6, 1/3, 1/2, 2/3, 5/6, 1
# og
# 1/12, 1/6, 1/4, 1/3, 5/12, 1/2, 7/12, 2/3, 3/4, 5/6, 11/12 og 1.
# 
# Alle de værdier for n=2, der er lig med værdier for n=1, vil være blokeret.
# 
# n=3:
# 
# 1:18
# 1/18, 2/18, 3/18 4/18, 5/18
# 
# brokerne(1:2)
# 

Hvis vi nu kun skal betrage en 6-del af dem.


138/6

brokerne <- function(n){
   list((1:(n))/(n*6))
 }

brokerne(2)
library(dplyr)
brokerne(1)
t <- 16
n <- 1:t
t5 <- n %>% 
  as.data.frame() %>% 
  rename(n=".") %>% 
  rowwise() %>% 
  mutate(p=brokerne(n)) %>% 
  mutate(l=length(p))


t5$o <- 0

for(i in 2:t){
  t5$o[i] = sum(unlist(t5[i,2]) %in% unlist(t5[1:(i-1),2]))
}

sum(t5$o)

6,12,24,30,45

2+3+4+5+6+7+8+9
eulersPhi(5)

2 - 1
3 - 2
4 - 4
5 - 5
6 - 9
7 - 10
8 - 14
9 - 17
10 - 23
11 - 24
12 - 32
13 - 33
14 - 41
15 - 48
16 - 56

library(numbers)
eulersPhi(10)

eulersPhi(5)

t5

6*c(1, 2, 4, 5, 9, 10, 14, 17, 23, 24,32,33)
23
24
32
33
41
48
56

t5
# 
# 1:1*6/6
# 
# n1 <- brokerne(1)
# n2 <- brokerne(2)
# n3 <- brokerne(3)
# n4 <- brokerne(4)
# n5 <- brokerne(5)
# 
# r1 <- length(n1)
# r2 <- length(setdiff(n2,n1))
# r3 <- length(setdiff(n3, c(n1,n2)))
# r4 <- length(setdiff(n4, c(n1,n2,n3)))
# r5 <- length(setdiff(n5, c(n1,n2,n3,n4)))
# 
# antalPunkter <- length(n1) +
#   length(n2) +
#   length(n3) +
#   length(n4) +
#   length(n5) 
# 
# r1 + r2 + r3 + r4 + r5
# 
# Antalet af punkter for given n:
#   (1:n)
# 1:1
# length(n1)
# 1: 6
# 2: 6 + 2*6 = 18
# 3: 18 + 
  
test <- list(n=1:5, p = numeric())
test
