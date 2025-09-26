425

Two positive numbers A and B are said to be connected (denoted by "A ↔ B") if one of these conditions holds:
  (1) A and B have the same length and differ in exactly one digit; for example, 123 ↔ 173.
(2) Adding one digit to the left of A (or B) makes B (or A); for example, 23 ↔ 223 and 123 ↔ 23.

We call a prime P a 2's relative if there exists a chain of connected primes between 2 and P and no prime in the chain exceeds P.

For example, 127 is a 2's relative. One of the possible chains is shown below:
  2 ↔ 3 ↔ 13 ↔ 113 ↔ 103 ↔ 107 ↔ 127
However, 11 and 103 are not 2's relatives.

Let F(N) be the sum of the primes ≤ N which are not 2's relatives.
We can verify that F(10^3) = 431 and F(10^4) = 78728.



Find F(10^7).

library(numbers)
primtal <- Primes(10000)

Vi kan nok med fordel skrive en funktion, der tester om to tal er forbundet.

En af to betingelser skal være opfyldt. Samme længde, og kun et ciffers forskel.
x <- 103
y <- 113

den anden: Hvis vi kan tilføje et ciffer til venstre for A og få B er de connected.
Hvis vi kan tilføje et ciffer til venstre for B og få A, er de connected.

Så forskellen på nchar(x)-nchar(y) skal være en.


C <- function(x,y){
  res <- F
  if(nchar(x)==nchar(y)){
    if(sum(unlist(str_split(x,"")) != unlist(str_split(y,"")))==1){
      res <- T
    }
  }
  if(abs(nchar(x)-nchar(y))==1){
    if((y-x)%%10^(max(nchar(c(x,y)))-1)==0){
      res <- T
    }
  }
  return(res)
}

C(2,2)

x<- 3
y <- 2
unlist(str_split(x,"")) != unlist(str_split(y,""))

length(primtal)

library(purrr)


forbindelser <- expand.grid(primtal,primtal) %>% 
  as.data.frame() %>% 
  mutate(connected = map2(Var1, Var2, C)) %>% 
  filter(connected==T) %>% 
  select(-connected) 

det bliver jeg nødt til at generere hurtigere.

primtal %>% 
  enframe(name=NULL)

for et givet tal - hvad kan den forbindes til?
x <-   13
de tal der kan skabe 13, ved at sætte et tal foran (det er søreme kun 3)
x%%10^(nchar(x)-1)
Alle de tal der kan skabes ved at sætte et tal foran 13
(1:9)*10^nchar(x)+x

Og så alle de tal, der kan skabes ved at ændre et af cifrene i x

det er mere tricky.

for(i in 1:(nchar(x))){
  print(x%%10^(i-1))
}

123%/%10%%10


eg. 


View(forbindelser)

get_forbindelser <- function(x,m){
  forbindelser[which(forbindelser$Var1==x & forbindelser$Var2<=m),]$Var2
}

m <- primtal[167]


forbund <- get_forbindelser(m,m)
res <- numeric()
for(i in forbund){
  res <- c(res, get_forbindelser(i,m))
}

forbund <- unique(res)

2 %in% forbund

allerede_forbundet <- c(2,3,5,7)



forbundet <- function(m){
  forbund <- get_forbindelser(m,m)
  j <- 0
  svar <- F
  while(j<10){
    res <- numeric()
    for(i in forbund){
      res <- c(res, get_forbindelser(i,m))
    }
    forbund <- unique(res)
    if(any(forbund %in% allerede_forbundet)){
      svar <- T
      break()
    }
    j <- j+1
    
  }
  if(2 %in% forbund){
    allerede_forbundet <- c(m, allerede_forbundet)
    svar <- T
  }
  return(svar)
}

forbundet(103)

testeren <- primtal %>% 
  enframe(name=NULL) %>% 
  mutate(to_forbundet=map(value, forbundet))



testeren %>% 
  unnest() %>% 
  filter(!to_forbundet) %>% 
  summarise(svar=sum(value)-2)


get_forbindelser(103,127)

dette er de tal der er to_forbundet
forbindelser[forbindelser$to_forbundet,]$Var1


C(127,977)
x <- 127
y <- 977
sum(unlist(str_split(x,"")) != unlist(str_split(y,"")))

    