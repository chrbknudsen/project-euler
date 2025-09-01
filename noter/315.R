# 315
# Digital roots er sådan set bare tværsummen.
# Hm. det giver ikke det rigtige resultat...
library(numbers)

kand <- Primes(10^7, 2*10^7)


x <- c(137,22)

digroot <- function(x){
  unlist((lapply((lapply(str_split(x,""), as.numeric)),sum)))
}
kand %>% 
  enframe() %>% 
  mutate(tværsum=digroot(value)) 

kost <- c(0,6,2,5,5,4,5,6,4,7,6)
names(kost) <- c(" ", as.character(0:9))

kost

DB <- rep(0,7)
names(DB) <- LETTERS[1:7]

D0 <- c(1,1,1,0,1,1,1)
names(D0) <- LETTERS[1:7]
D1 <- c(0,0,1,0,0,1,0)
names(D1) <- LETTERS[1:7]
D2 <- c(1,0,1,1,1,0,1)
names(D2) <- LETTERS[1:7]

D3 <- c(1,0,1,1,0,1,1)
names(D3) <- LETTERS[1:7]

D4 <- c(0,1,1,1,0,1,0)
names(D4) <- LETTERS[1:7]

D5 <- c(1,1,0,1,0,1,1)
names(D5) <- LETTERS[1:7]

D6 <- c(1,1,0,1,1,1,1)
names(D6) <- LETTERS[1:7]

D7 <- c(1,1,1,0,0,1,0)
names(D7) <- LETTERS[1:7]

D8 <- rep(1,7)
names(D8) <- LETTERS[1:7]

D9 <- c(1,1,1,1,0,1,1)
names(D9) <- LETTERS[1:7]

sum(xor(D1, DB))
sum(xor(D3, D1))
sum(xor(D7, D1))

sum(xor(DB, D1))
sum(xor(DB, D3))
sum(xor(DB, D7))




fra_til_1 <- function(x,y){
  # 1 sluk x
  # 2 tænd y
  x <- as.character(x)
  y <- as.character(y)
  res <- sum(kost[unlist(str_split(x,""))])   # det er hvad det koster at slukke x
  res <- res + sum(kost[unlist(str_split(y,""))])   # det er hvad det koster at slukke y, lagt sammen med tidligere res
  return(res)
}

fra_til_1("137", "11") + fra_til_1("11", "2")

fra_til_2 <- function(x,y){
  x <- as.character(x)
  y <- as.character(y)
  if(nchar(y)<nchar(x)){
    y <- str_pad(y, nchar(x), "left")
  }
  if(nchar(x)<nchar(y)){
    x <- str_pad(x, nchar(y), "left")
  }
  x <- unlist(str_split(x, ""))
  y <- unlist(str_split(y,""))
  x <- str_replace(x, " ", "B")
  x <- str_c("D",x)
  y <- str_replace(y, " ", "B")
  y <- str_c("D", y)
  res <- 0
  for(i in 1:length(y)){
    res <- res + sum(xor(eval(as.name(x[i])), eval(as.name(y[i]))))
  }
  return(res)
}

x <- " "

fra_til_2(11,2)

fra_til_1(" ", "137") + fra_til_1(137, 11) + fra_til_1(11,2) + fra_til_1("2"," ")

fra_til_2(" ", "137") + fra_til_2(137, 11) + fra_til_2(11,2) + fra_til_2("2", " ")


tværsummer
# max er 64. så eftersom vi kun er interesserede i forskellen på de to metoder, kan vi nu trunkere de originale primtal. De er alle
# på 8 cifre. Men det er kun de sidste to der reelt er interessante.

pared <- kand %>% 
  enframe() %>% 
  mutate(tværsum=digroot(value)) %>% 
  mutate(value = as.character(value)) %>% 
  mutate(value = str_sub(value, 7))

pared <- pared %>% 
  select(-name)

korte <- pared %>% 
  filter(tværsum < 10) %>% 
  rowwise() %>% 
  mutate(SAM = fra_til_1(value, tværsum)) %>% 
  mutate(MAX = fra_til_2(value, tværsum))



resten <- pared %>% 
  filter(tværsum > 9)

resten %>% 
  group_by(value, tværsum) %>% 
  summarise(antal = n()) %>% 
  rowwise() %>% 
  mutate(SAM = fra_til_1(value, tværsum)) %>% 
  mutate(MAX = fra_til_2(value, tværsum)) %>% 
  mutate(SAMsum = SAM*antal) %>% 
  mutate(MAXsum = MAX*antal) %>% 
  ungroup() %>% 
  summarise(sumSAM = sum(SAMsum), sumMAX = sum(MAXsum))

korte %>% 
  ungroup() %>% 
  summarise(sumSAM = sum(SAM), sumMAX = sum(MAX))

sumMAX <- 3160383 + 1715
sumSAM <-  11426283 + 2691

delresultat <- sumSAM - sumMAX

resten <- resten %>% 
  select(tværsum) %>% 
  mutate(nytvær = digroot(tværsum))

de_korte <- resten %>% 
  filter(nytvær < 10)

de_korte %>% 
  group_by(tværsum, nytvær) %>% 
  summarise(antal = n()) %>% 
  rowwise() %>% 
  mutate(SAM = fra_til_1(tværsum, nytvær)) %>% 
  mutate(MAX = fra_til_2(tværsum, nytvær)) %>% 
  mutate(SAMsum = SAM*antal) %>% 
  mutate(MAXsum = MAX*antal) %>% 
  ungroup() %>% 
  summarise(sumSAM = sum(SAMsum), sumMAX = sum(MAXsum))

de korte er nu regnet med:
del2resultat <-   5818192 - 3103574

resten <- resten %>% 
  filter(nytvær > 9)

resten %>% 
  group_by(tværsum, nytvær) %>% 
  summarise(antal = n()) %>% 
  rowwise() %>% 
  mutate(SAM = fra_til_1(tværsum, nytvær)) %>% 
  mutate(MAX = fra_til_2(tværsum, nytvær)) %>% 
  mutate(SAMsum = SAM*antal) %>% 
  mutate(MAXsum = MAX*antal) %>% 
  ungroup() %>% 
  summarise(sumSAM = sum(SAMsum), sumMAX = sum(MAXsum))

del3resultat <- 3262183 - 1232625

resten <- resten %>% 
  select(nytvær) %>% 
  rename(tværsum = nytvær) %>% 
  mutate(nytvær= digroot(tværsum))

resten %>% 
  group_by(tværsum, nytvær) %>% 
  summarise(antal = n()) %>% 
  rowwise() %>% 
  mutate(SAM = fra_til_1(tværsum, nytvær)) %>% 
  mutate(MAX = fra_til_2(tværsum, nytvær)) %>% 
  mutate(SAMsum = SAM*antal) %>% 
  mutate(MAXsum = MAX*antal) %>% 
  ungroup() %>% 
  summarise(sumSAM = sum(SAMsum), sumMAX = sum(MAXsum))


del4resultat <-  1866447 - 1252257


delresultat + del2resultat + del3resultat + del4resultat
