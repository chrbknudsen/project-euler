174 

følger ret enkelt når 173 er løst på den bøvlede måde

l <- 1000000

Nedre grænse for y: 3
Øvre grænse for y: l/4+1 (så kan den ikke blive større)

y_max = l/4+3



Øvre grænse for den indre: y-2
Nedre grænse for indre 1


Når y^2 > l kan vi sætte en ny nedre grænse for i.

f <- function(x,y){
  seq(x,y,by=2)
}

library(tidyverse)
library(purrr)
t174 <- 3:y_max %>% 
  enframe(name=NULL) %>% 
  rename(y=value) %>% 
  mutate(i_max = y-2) %>% 
  mutate(i_min = if_else(y^2>l, floor(sqrt(y^2-l)), 2)) %>% 
  mutate(i_min = if_else(y%%2==0 & i_min%%2!=0 & i_min>1, i_min-1, i_min)) %>% 
  mutate(i_min = if_else(y%%2!=0 & i_min%%2==0, i_min-1, i_min)) %>% 
  mutate(i = map2(i_min, i_max, f)) %>% 
  unnest() %>% 
  mutate(antal_tiles = y^2 - i^2) %>% 
  filter(antal_tiles < l+1) 

t174 %>% 
  select(antal_tiles) %>% 
  table() %>% 
  enframe() %>% 
  filter(value %in% 1:10) %>% 
  nrow()
