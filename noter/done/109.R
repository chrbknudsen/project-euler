109

Man kan ramme forbi. Det er 0 

Og så er der S1, D1 og T1 til og med S20

Samt single og dobbelt bull.

library(dplyr)
library(tidyverse)
library(purrr)

sdt <- c("S", "D", "T")
tal <- 1:20
score <- expand.grid(sdt,tal) %>% 
  mutate(value=as.integer(factor(Var1))*Var2) %>% 
  mutate(name=paste(Var1,Var2,sep="")) %>% 
  select(value, name)

score

i <- nrow(score) + 1
score[i,1] <- 0
score[i,2] <- "SM"
i <- nrow(score) + 1
score[i,1] <- 25
score[i,2] <- "SB"
i <- nrow(score) + 1
score[i,1] <- 50
score[i,2] <- "DB"

score

score

test <- score$value
names(test) <- score$name



checkouts <- expand.grid(score$name,score$name,score$name, stringsAsFactors = F)

library(stringr)


checkouts %>% 
  filter(str_sub(checkouts$Var3,1,1)=="D")

Så nærmer vi os. Men... Rækken D6 T2, D1 er at regne som identisk med T2 D6 D1. Hvordan piller jeg dem ud?
  
library(purrr)  
  
tester <- checkouts %>% 
  filter(str_sub(checkouts$Var3,1,1)=="D")  %>% 
  mutate(noget = map2(Var1, Var2, function(x,y) paste0(unlist(sort(c(x,y) ) ), collapse=","))) %>% 
  mutate(noget = unlist(noget))

test2 <- tester %>% 
  group_by(Var3) %>% 
  distinct(noget) %>% 
  ungroup() %>% 
  separate(noget, into=c("Var1", "Var2"), sep=",", remove=F) %>% 
  mutate(V1 = test[Var1]) %>% 
  mutate(V2 = test[Var2]) %>% 
  mutate(V3 = test[Var3]) 
 
View(test2)

test2 %>% 
  mutate(score = V1+V2+V3) %>% 
  filter(score < 100) %>% 
  nrow()

test3 <- test2 %>% 
  mutate(score = V1+V2+V3)



View(test3)


test["DB"]

library(tidyverse)
expand.grid(1:10, 1:10) %>% 
  mutate(noget = map2(Var1, Var2, function(x,y) sort(c(x,y)) ))

test["D1"]
