112

Er et tal bouncy?
  
Hvordan tester man for det?
  
Det er bouncy, hvis det hverken er decreasing eller increasing.
123444567 
Er increasing
test <- "123444567" 

library(stringr)
library(tidyverse)

test <- unlist(str_split(test,""))
test[-length(test)] <= test[-1]
test[-length(test)]

test[-1] >= test[-length(test)]

increasing <- function(x){
  if(x<10){return(FALSE)}
  test <- unlist(str_split(x,""))
  all(test[-1] >= test[-length(test)])
}

decreasing <- function(x){
  if(x<10){return(FALSE)}
  test <- unlist(str_split(x,""))
  all(test[-1] <= test[-length(test)])
}

increasing(155349)
decreasing(155349)


test


i <- 10
t <- 0

decreasing(98)

while(t/i < 0.99){
  t <- t + (!increasing(i) & !decreasing(i))
  if(100*t == 99*i){
    print(i)
    break()
  }
  i <- i +1
}

options(scipen=999)
t/i



test <- 1:540

test <- test %>% 
  enframe(name=NULL)


test$incr <- apply(test,1, increasing)

test <- test %>% 
  rowwise() %>% 
  mutate(decr = decreasing(value))

test <- test %>% 
  mutate(bounc = !incr & !decr) 

View(test)
test[1:11,4] <- F

test %>% 
  ungroup() %>% 
  mutate(boun = as.numeric(bounc)) %>% 
  mutate(cum_boun = cumsum(boun)) %>% 
  mutate(ratio = cum_boun/value) %>% 
  View()

test %>% 
  filter(value>100) %>% 
  filter(!incr) %>% 
  filter(!decr) %>% 
  View()

test$decr <- apply(test$value, 1, decreasing)

View(test)

View(test)

increasing(i)

