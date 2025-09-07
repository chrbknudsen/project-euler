# 70

library(numbers)
# eulersPhi(n). Hvonår er det en permutaiton af n?
#   Hvor er minimumet af dem for n<10^7
# 
# Eller:
#   Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of positive numbers less than or equal to n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
# The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.
# 
# Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.
# 
# Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.
# 
# Det at det skal være et minimum, betyder at phi(n) skal være stor. 
# Hvis nu jeg gætter... n skal ikke være lige. For de har ret små phi. Jeg gætter også på at det er et primtal.
# 

# Der skal være lige mange cifre i phi(n) og n
# 
# Hvor mange cifre er der i phi(n)?
# 
# produktformlem fortæller, at hvis vi finder alle primtal der dividerer n. 

t <- 1:(10^7)
eulersPhi(9999996)

# Vi er ude efter et minimum af n/phi(n). Efter produktformlen, er det derfor produktet af alle primtal der dividerer n, divideret med 1, trukket fra 1
# og ganget sammen. 
# 
# Det inverse til det produkt, skal minimeres. Hvilket betyder at produktet i sig selv skal maksimeres. Eftersom alle de tal der skal ganges sammen er 
# mindre end 0, skal vi have så få led som muligt i det. og primtallene p skal være så store som muligt.
# eulers phi af et primtal x er lig x-1. Og totienten af et primtal vil derfor aldrig være en kandidat - da den ikke kan være en permutation.

OK. Hvad kan det så være? Det er så et tal der er produktet af mindst to primtal. 

{\displaystyle \varphi (n)=n\prod _{p\mid n}\left(1-{\frac {1}{p}}\right),}

Jo færre og jo større primtal der dividerer n, jo større er produktet, og jo mindre er det inverse til produktet. 

Lad os prøve med to primfaktorer.


Vi skal have fat på alle primtal mindre end 10^7 /2

test <- Primes(ceiling(sqrt(10^7)*10))

tiltekst <- function(x){
  str_split(x,"") %>% 
    map(sort) %>% 
    map(paste0, collapse="") %>% 
    unlist()
}

tiltekst(c(1060, 1016))

kand <- expand.grid(test, test)

kand %>% 
  mutate(v = Var1*Var2) %>% 
  filter(v < 10^7) %>% 
  mutate(phi = v*(1-1/Var1)*(1-1/Var2)) %>% 
  mutate(vs = tiltekst(v)) %>% 
  mutate(phis = tiltekst(phi)) %>% 
  filter(vs == phis) %>% 
  mutate(brok = v/phi) %>% 
  arrange(brok)




7026037
9999999
filtret <- function(x,y) x*y<10^7

cross2(test,test, .filter=filter)

combn(test,2)
  
test <- Primes(10^7)  

test[test>5*(10^6)]


test[test>5*(10^6)] %>% 
  enframe(name=NULL, value="p") %>% 
  mutate(phi = map(p, eulersPhi))


10^7-1
log10(79180)
log10(87109)
library(numbers)
library(tidyverse)
library(foreach)

eulers phi:
  
  Produktfunktionen:
  phi(n )= 
  
  φ(mn) = φ(m) φ(n)

φ(n) = n gange det der produkt.
Det vi skal finde, er n/ φ(n)

så det vi ønsker at minimere, er 1/produkttingen.

den er mindst, når produkttingen er størst.


Så få og så store faktorer som muligt.

1-1/p ganget sammen for alle de p primfaktorer 


t <- 1:10
t
pf <- Vectorize(primeFactors)
(pf(t))
prod(1-1/primeFactors(value))*value

1:10^7 %>% 
  enframe() %>% 
  rowwise() %>% 
  mutate(P=prod(1-1/primeFactors(value))*value)

1-1/primeFactors(5)

eulersPhi(2)
eulersPhi(5)
primeFactors(16)
eulersPhi(10)
GCD(2,5)
eulersPhi(87109)

factors(87109)

eulersPhi(16)

t <- table(primeFactors(340))

test <- 1:10^7
phi <- Vectorize(eulersPhi)

test <- test %>% 
  enframe() %>% 
  as.data.frame()

test$P <- 0.1
test

for(i in 1:nrow(test)){
  print(i)
  test[i,3] <- eulersPhi(test[i,2])
}

test

i <- 10
eulersPhi(test[i,2])

  mutate(P= phi(value)) %>% 
  View()

library(numbers)

eulersPhi(8)

Phi <- Vectorize(eulersPhi)
test <- seq(1, 10^7, by=2) %>% 
  enframe() %>% 
  mutate(phi = NA)
head(test)

test

# for(i in 1:nrow(test)){
#   test[i,3] <- eulersPhi(i)
# }

str(test)

foreach(i=1:nrow(test)) %do%
  test[i,3] <- eulersPhi(test[i,2])
is.numeric(test[1,2])

test <- 1:10^7 %>% 
  enframe() %>% 
  mutate(phi = NA)

for(i in 1:nrow(test)){
  test[i,]$phi <- eulersPhi(test[i,]$value)
}



View(test)

sum(!is_numeric(test$value))

i <- 381
is.numeric(test$value)
test[i,2]
eulersPhi(759)
test[38]
eulersPhi(test[i,2])

Godt så. er value en permutation af phi?

test %>% 
  filter(floor(log10(abs(value)))==floor(log10(abs(phi))))
  
    filter(all(sort(unlist(str_split(as.character(test$value),""))) == sort(unlist(str_split(as.character(test$phi),"")))))

Return floor(log10(abs(x))) + 1;

  
  
  all(sort(unlist(str_split(as.character(test[10,2]),""))) == sort(unlist(str_split(as.character(test[10,3]),""))))
