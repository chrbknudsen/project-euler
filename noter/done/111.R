111

library(numbers)

Mit svar er naturligvis forkert. Jeg læste ikke opgaven rigtig. Her finder jeg naturligvis alle tal der har 8 ens cifre i 
rækkefølge. Men læser man opgaven korrekt - får man øje på 1511, der har 3 repeated 1-taller, og er et validt tal. 
Så der er mange tal jeg mangler at teste. og hvordan gør jeg så det?
  


S9 <- function(n){
  base <- 1111111111
  korr <- 10^(0:9)
  test <- numeric()
  for(e in 0:9){
    test <- c(test, (base - korr)*n + e*korr)
  }
  test <- test[test>10^9]
  res <- numeric()
  for(i in 1:length(test)){
    if(isPrime(test[i])){
      res <- c(res, test[i])
    }
  }
  
  return(sum(res))
}
  
Hvis der S9(n) returnerer 0, så er der ingen primtal med 9 gentagne n.
Så skal der ledes efter dem med 8.


Hvordan genererer vi dem?
  
Vi skal have alle kombinationer af korr

library(tidyverse)
n <- 8
S8 <- function(n){
  base <- 1111111111
  test <- numeric()
  korr <- 10^(0:9)
  korr <- combn(korr, 2) %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(base = (base - V1 - V2)*n)
  for(i in 0:9){
    for(j in 0:9){
      test <- c(test, korr %>% 
                  transmute(kand = base + i*V1+ j*V2) %>% 
                  filter(nchar(kand)==10) %>% 
                  filter(kand%%2!=0) %>% 
                  filter(kand%%5!=0) %>% 
                  .$kand)
    }
  }
  res <- numeric()
  for(i in 1:length(test)){
    if(isPrime(test[i])){
      res <- c(res, test[i])
    }
  }
  
  return(sum(res))
  
  
}

hm <- S8(0)

S8(0) + S9(1) + S8(2) + S9(3) + S9(4) + S9(5) + S9(6) + S9(7) + S8(8) + S9(9)

d       M(10,d)       N(10,d)
-------------------------------
  0       8                8  korrekt
1       9                11  korrekt
2       8                34 - her er der fejl
3       9                7  korrekt
4       9                1 korrekt
5       9                1 korrekt
6       9                1 korrekt
7       9                9 korrekt
8       8                24 - her er der fejl
9       9                8  korrekt



svaret <- 0

for(n in 0:9){
  del <- 0
  del <- S9(0)
  if(del == 0) {del <- S8(n)}
  svaret <- svaret + del
}


188%%2

hm <- S8(8)

for(i in 1:length(hm)){
  print(isPrime(hm[i]))
}

korr

for(i in 0:9){
  for(j in 0:9){
    test <- c(test, korr %>% 
      transmute(kand = base + i*V1+ j*V2) %>% 
      filter(nchar(kand)==10) %>% 
      .$kand)
  }
}

combn(korr, 2) %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(base = (base - V1 - V2)*n)

Nu skal vi så tage alle kombinationer af tallene 

combn(korr, 2) %>% 
  t() %>% 
  as.data.frame()




0       8                8
1       9                11  X
2       8                34
3       9                7
4       9                1
5       9                1
6       9                1
7       9                9
8       8                24
9       9                8


position <- 1

test <- numeric()

n <- 8

base <- 1111111111
        
korr <- 10^(0:9)

e <- 7

(base - korr)*n + e*korr


nchar(n*base - korr*n)

for(p in 1:10){
  for(n in 0:9){
    test <- c(test, base%%(10^(10-p))+
               base%/%10^(10-p+1)*10^(10-p+1)+
               n*10^(10-p)
    )
  }
}

test <- test[test>1000000000]

res <- numeric()
for(i in 1:length(test)){
  if(isPrime(test[i])){
    res <- c(res, test[i])
  }
}



