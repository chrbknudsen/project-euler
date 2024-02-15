164

How many 20 digit numbers n (without any leading zero) exist such that no three consecutive digits of n have a sum greater than 9?
  
  
naeste <- function(x,y=0){
  return(0:(9-x-y))
}


l <- 20

library(dplyr)
library(purrr)
library(rlang)
1:9 %>% 
  enframe(name=NULL) %>% 
  rename(V1 = value) %>%
  mutate(V2 = map(V1, naeste)) %>% 
  unnest(V2) %>% 
  mutate(V3 = map2(V1,V2, naeste)) %>% 
  unnest(V3) %>% 
  mutate(V4 = map2(V2,V3, naeste)) %>% 
  unnest(V4) %>% 
  mutate(V5 = map2(V3,V4, naeste)) %>% 
  unnest(V5)  %>% 
  mutate(V6 = map2(V4,V5, naeste)) %>% 
  unnest(V6) %>% 
  mutate(V7 = map2(V5,V6, naeste)) %>% 
  unnest(V7) %>% 
  mutate(V8 = map2(V6,V7, naeste)) %>% 
  unnest(V8) %>% 
  mutate(V9 = map2(V7,V8, naeste)) %>% 
  unnest(V9)


V4 - 990
V6 - 27588
V8 - 783057
V9 - 4129851

start <- 1:9 %>% 
  enframe(name=NULL) %>% 
  rename(V1 = value) %>%
  mutate(V2 = map(V1, naeste)) %>% 
  unnest(V2) %>% 
  mutate(V3 = map2(V1,V2, naeste)) %>% 
  unnest(V3)%>% 
  mutate(V4 = map2(V2,V3, naeste)) %>% 
  unnest(V4)

start <- start %>% 
  group_by(V3,V4) %>% 
  mutate(antal = n()) %>% 
  select(-V1, -V2) %>% 
  distinct() %>% 
  ungroup()



videre <- start %>%
  mutate(V5 = map2(V3,V4, naeste)) %>% 
  unnest(V5) %>% 
  mutate(V6 = map2(V4,V5, naeste)) %>% 
  unnest(V6)


videre <- videre %>% 
  select(antal,V5,V6) %>% 
  group_by(V5,V6) %>% 
  mutate(nyt_antal = sum(antal)) %>% 
  select(nyt_antal , V5,V6) %>% 
  distinct() %>% 
  ungroup() %>% 
  rename(antal=nyt_antal)


videre8 <- videre %>% 
  mutate(V7 = map2(V5,V6, naeste)) %>% 
  unnest(V7) %>% 
  mutate(V8=map2(V6,V7, naeste)) %>% 
  unnest(V8) %>% 
  select(antal, V7, V8) %>% 
  group_by(V7,V8) %>% 
  mutate(nyt_antal = sum(antal)) %>% 
  select(nyt_antal, V7,V8) %>% 
  distinct() %>% 
  ungroup() %>% 
  rename(antal=nyt_antal) 


videre10 <- videre8 %>% 
  mutate(V9 = map2(V7,V8, naeste)) %>% 
  unnest(V9) %>% 
  mutate(V10=map2(V8,V9, naeste)) %>% 
  unnest(V10) %>% 
  select(antal, V9, V10) %>% 
  group_by(V9,V10) %>% 
  mutate(nyt_antal = sum(antal)) %>% 
  select(nyt_antal, V9,V10) %>% 
  distinct() %>% 
  ungroup() %>% 
  rename(antal=nyt_antal) 

videre12 <- videre10 %>% 
  mutate(V11 = map2(V9,V10, naeste)) %>% 
  unnest(V11) %>% 
  mutate(V12=map2(V10,V11, naeste)) %>% 
  unnest(V12) %>% 
  select(antal, V11, V12) %>% 
  group_by(V11,V12) %>% 
  mutate(nyt_antal = sum(antal)) %>% 
  select(nyt_antal, V11,V12) %>% 
  distinct() %>% 
  ungroup() %>% 
  rename(antal=nyt_antal) 

videre14 <- videre12 %>% 
  mutate(V13 = map2(V11,V12, naeste)) %>% 
  unnest(V13) %>% 
  mutate(V14=map2(V12,V13, naeste)) %>% 
  unnest(V14) %>% 
  select(antal, V13, V14) %>% 
  group_by(V13,V14) %>% 
  mutate(nyt_antal = sum(antal)) %>% 
  select(nyt_antal, V13,V14) %>% 
  distinct() %>% 
  ungroup() %>% 
  rename(antal=nyt_antal) 




videre16 <- videre14 %>% 
  mutate(V15 = map2(V13,V14, naeste)) %>% 
  unnest(V15) %>% 
  mutate(V16=map2(V14,V15, naeste)) %>% 
  unnest(V16) %>% 
  select(antal, V15, V16) %>% 
  group_by(V15,V16) %>% 
  mutate(nyt_antal = sum(as.numeric(antal))) %>% 
  select(nyt_antal, V15,V16) %>% 
  distinct() %>% 
  ungroup() %>% 
  rename(antal=nyt_antal) 


videre18 <- videre16 %>% 
  mutate(V17 = map2(V15,V16, naeste)) %>% 
  unnest(V17) %>% 
  mutate(V18=map2(V16,V17, naeste)) %>% 
  unnest(V18) %>% 
  select(antal, V17, V18) %>% 
  group_by(V17,V18) %>% 
  mutate(nyt_antal = sum(as.numeric(antal))) %>% 
  select(nyt_antal, V17,V18) %>% 
  distinct() %>% 
  ungroup() %>% 
  rename(antal=nyt_antal) 

videre20 <- videre18 %>% 
  mutate(V19 = map2(V17,V18, naeste)) %>% 
  unnest(V19) %>% 
  mutate(V20=map2(V18,V19, naeste)) %>% 
  unnest(V20) %>% 
  select(antal, V19, V20) %>% 
  group_by(V19,V20) %>% 
  mutate(nyt_antal = sum(as.numeric(antal))) %>% 
  select(nyt_antal, V19,V20) %>% 
  distinct() %>% 
  ungroup() %>% 
  rename(antal=nyt_antal) 

options(scipen=999)
sum(videre20$antal)

V8 - 783057
378158756814587
  
videre %>% 
  select(antal, V5, V6) %>% 
  group_by(V5,V6) %>% 
  mutate(nyt_antal = n()) %>% 
  mutate(nyere_antal = antal+nyt_antal) %>% 
  select(V5,V6, nyere_antal) %>% 
  distinct() %>% 
  ungroup() %>% 
  select(V5,V6, nyere_antal) %>% 
  summarise(noget = sum(nyere_antal))

View(endnu_videre)

group_by(V5,V6) %>% 
  select(V5,V6, antal) %>% 
  distinct() %>% 
  ungroup

videre


videre %>% 
  mutate(V7 = map2(V5,V6, naeste)) %>% 
  unnest(V7) %>% 
  mutate(V8 = map2(V6, V7, naeste)) %>% 
  unnest(V8)



