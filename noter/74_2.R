74 take 2

listen <- 1:1000000

library(tidyverse)

fak_dig_sum <- function(x){
  unlist(map(map(map(map(map(x, str_split, ""), unlist),as.numeric),factorial),sum))
}


kandidater <- listen %>% 
  enframe(name=NULL) %>% 
  mutate(første = fak_dig_sum(value)) %>% 
  group_by(første) %>% 
  mutate(antal=n()) %>% 
  select(første,antal) %>% 
  ungroup() %>% 
  distinct()
