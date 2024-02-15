#90

Ok. Vi ved at den første kube må have tallet 0.
Det betyder også, at den anden kube er nødt til at have tallene 1,4,6 (og 9)

Det er distinkte arrangementer vi leder efter. Hm. 

kube1 har 0. samt fem andre tal. Et af dem kunne være 0. Der er heller ikke noget til hinder for at både 6 og 9 kan være der. 

Så hvor mange måder kan vi vælge 5 tal ud af 10?
  
10*9*8*7*6

30240

På den anden kube, skal vi have tallene 1 og 4. samt et af tallene 6 og 9.

det vil sige, at vi skal være 4 tal ud af 8 mulige.

8*7*6*5



For de seks tal skal være forskellige.

På kube2, kan vi udelukke de sæt af tal, der hverken indeholder tallene 6 eller 9.

library(dplyr)
combn(c(0,2,3,5,6,7,8,9),4) %>% 
  t() %>% 
  as_data_frame() %>% 
  mutate(V5 = 1, V6=4) %>% 
  rowwise() %>% 
  filter(6 %in% c(V1,V2, V3, V4) |9 %in% c(V1,V2, V3, V4)) %>% 
  ungroup() -> kube2

combn(1:9,5) %>% 
  t() %>% 
  as_data_frame() %>% 
  mutate(V6 = 0) -> kube1

Nu har vi to kuber.
Vi ved, for det har vi sikret os i konstruktionen af dem, at de kan danne 01, 04 og 09.

01 kan dannes ved en hvilken som helst af de 126 distinkte arrangementer i kube1, samt alle de arrangementer af kube 2, som kan indeholder et 1 tal 

antallet af kube1(0) ganget med antallet af kube2(1) + antallet af kube2(0) ganget med antallet af kube1(1)


Vi skal nok bruge en funktion, der tager en kube, og et tal, og fortæller hvor mange gange der indeholder det tal.

nnk1 <- function(kube, x){
  kube %>% 
    rowwise() %>% 
    filter(x %in% c(V1, V2, V3, V4, V5, V6)) %>% 
    ungroup() %>% 
    summarise(n=n()) %>% 
    as.integer()
}


nnk <- function(kube, x){
  res <- kube %>% 
    rowwise() %>% 
    filter(x %in% c(V1, V2, V3, V4, V5, V6)) %>% 
    ungroup() %>% 
    summarise(n=n()) %>% 
    as.integer()
  if(x %in% c(6,9)){
    res <- kube %>% 
      rowwise() %>% 
      filter(6 %in% c(V1, V2, V3, V4, V5, V6) | 9 %in% c(V1, V2, V3, V4, V5, V6) ) %>% 
      ungroup() %>% 
      summarise(n=n()) %>% 
      as.integer()
  }
  return(res)
}


kube1 <- combn(0:9,6) %>% 
  t() %>% 
  apply(1, as.list)


kuber <- expand.grid(kube1,kube1)

0 %in% unlist(kuber$Var1[[1]])

kube1 <- kube2 <-  %>% 
  as_data_frame()


nrow(kube2)


nnk(kube2,6)

View(kube2)

men det bliver for højt. For de kuber der indeholder både 6 og 9. Og de er der jo nok. de kommer til at tælle med to gange.
Så. nnk skal laves om. den returnerer fint antallet af rækker der indeholder et 5 tal i en given kube.

Men vi skal håndtere 6 og 9.

k01 <- nnk(kube1,0) * nnk(kube2,1) + nnk(kube1,1) * nnk(kube2,0)
k04 <-   nnk(kube1,0) * nnk(kube2,4) + nnk(kube1,4) * nnk(kube2,0)
k09 <- nnk(kube1,0) * nnk(kube2,6) + nnk(kube1,0) * nnk(kube2,9) + nnk(kube1,6) * nnk(kube2,0) + nnk(kube1,9) * nnk(kube2,0)
k16 <- nnk(kube1,1)*nnk(kube2,6) + nnk(kube1,1)*nnk(kube2,9) 
k25 <- nnk(kube1,2)*nnk(kube2,5) + nnk(kube1,5)*nnk(kube2,2)
k36
k64
k81 <- nnk(kube1,8)*nnk(kube2,1) + nnk(kube1,1)*nnk(kube2,8) 



===============================================
  
library(dplyr)  
  
kube1 <- combn(0:9,6) %>% 
  t() %>% 
  apply(1, as.list)


kuber <- expand.grid(kube1,kube1)

i <- 1
unlist(kuber$Var1[[i]])
unlist(kuber$Var2[[i]])

t01(i)
t01 <- function(i){
  (0 %in% unlist(kuber$Var1[[i]]) & 1 %in% unlist(kuber$Var2[[i]])) | 
  (1 %in% unlist(kuber$Var1[[i]]) & 0 %in% unlist(kuber$Var2[[i]]))
}

t04 <- function(i){
  (0 %in% unlist(kuber$Var1[[i]]) & 4 %in% unlist(kuber$Var2[[i]])) | 
  (4 %in% unlist(kuber$Var1[[i]]) & 0 %in% unlist(kuber$Var2[[i]]))
}

t09 <- function(i){
  (0 %in% unlist(kuber$Var1[[i]]) & 6 %in% unlist(kuber$Var2[[i]])) | 
  (0 %in% unlist(kuber$Var1[[i]]) & 9 %in% unlist(kuber$Var2[[i]])) |
  (6 %in% unlist(kuber$Var1[[i]]) & 0 %in% unlist(kuber$Var2[[i]])) | 
  (9 %in% unlist(kuber$Var1[[i]]) & 0 %in% unlist(kuber$Var2[[i]])) 
}


t16 <- function(i){
    (1 %in% unlist(kuber$Var1[[i]]) & 6 %in% unlist(kuber$Var2[[i]])) | 
    (1 %in% unlist(kuber$Var1[[i]]) & 9 %in% unlist(kuber$Var2[[i]])) |
    (6 %in% unlist(kuber$Var1[[i]]) & 1 %in% unlist(kuber$Var2[[i]])) | 
    (9 %in% unlist(kuber$Var1[[i]]) & 1 %in% unlist(kuber$Var2[[i]])) 
}

t25 <- function(i){
  (2 %in% unlist(kuber$Var1[[i]]) & 5 %in% unlist(kuber$Var2[[i]])) | 
  (5 %in% unlist(kuber$Var1[[i]]) & 2 %in% unlist(kuber$Var2[[i]]))
}

t36 <- function(i){
    (3 %in% unlist(kuber$Var1[[i]]) & 6 %in% unlist(kuber$Var2[[i]])) | 
    (3 %in% unlist(kuber$Var1[[i]]) & 9 %in% unlist(kuber$Var2[[i]])) |
    (6 %in% unlist(kuber$Var1[[i]]) & 3 %in% unlist(kuber$Var2[[i]])) | 
    (9 %in% unlist(kuber$Var1[[i]]) & 3 %in% unlist(kuber$Var2[[i]])) 
}


t49 <- function(i){
    (4 %in% unlist(kuber$Var1[[i]]) & 6 %in% unlist(kuber$Var2[[i]])) | 
    (4 %in% unlist(kuber$Var1[[i]]) & 9 %in% unlist(kuber$Var2[[i]])) |
    (6 %in% unlist(kuber$Var1[[i]]) & 4 %in% unlist(kuber$Var2[[i]])) | 
    (9 %in% unlist(kuber$Var1[[i]]) & 4 %in% unlist(kuber$Var2[[i]])) 
}

t64 <- function(i){
    (4 %in% unlist(kuber$Var1[[i]]) & 6 %in% unlist(kuber$Var2[[i]])) | 
    (4 %in% unlist(kuber$Var1[[i]]) & 9 %in% unlist(kuber$Var2[[i]])) |
    (6 %in% unlist(kuber$Var1[[i]]) & 4 %in% unlist(kuber$Var2[[i]])) | 
    (9 %in% unlist(kuber$Var1[[i]]) & 4 %in% unlist(kuber$Var2[[i]])) 
}

t81 <- function(i){
  (8 %in% unlist(kuber$Var1[[i]]) & 1 %in% unlist(kuber$Var2[[i]])) | (1 %in% unlist(kuber$Var1[[i]]) & 8 %in% unlist(kuber$Var2[[i]]))
}

View(kuber)
class(kuber)


kuber$ok <- NA
for(i in 1:nrow(kuber)){
  if(t01(i) & t04(i) & t09(i) & t16(i) & t25(i) & t36(i) & t49(i) & t64(i) & t81(i)){
    kuber$ok[i] <- 1
  }
}

kuber %>% 
  filter(ok==1)


View(kuber)
sum(kuber$ok, na.rm=T)/2

og vi dividerer med 2 - fordi halvdelen af kombinationerne der dublerede.
kube1 og kube2 dur.
Men samme konfiguration af kube2 og kube1 optræder også i listen. og er ikke distinkt fra
kube1 og kube2


kuber %>% 
  rowwise() %>% 
  filter(isFALSE(all_equal(unlist(kuber$Var1), unlist(kuber$Var2)))) %>% 
  ungroup()

all_equal(unlist(kuber$Var1[2]), unlist(kuber$Var2[2]))
