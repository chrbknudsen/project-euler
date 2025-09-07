183

N er en positiv integer. Den kan vi dele i k lige store dele. N/k = r. Det fører til at N = rk

P er produktet af dem. P= r^k

M(N) er den maksimale P, for en given N. Dvs, at vi skal finde den største (N/k)^k. Eller N^k/k^k

Eg, N=11 har maks ved k=4 => M(N) = (11/4)^4. Eller 11^4/4^4 = 14641/256 = 57.19140625

Og det er en "terminerende" decimalbrøk.

N=8, har maks ved k=3 => M(N) = 8^3/3^3 = 512/27 som er en ikke-terminerende decimalbrøk.

Sæt nu D(N) = N hvis M(N) er non-terminerende, og D(N) = -N, hvis M(N) er terminerende.
Hvad er summen af D(N) for 5 ≤ N ≤ 10000 ?
  
  
Det der er problemet er dels at finde maks. Det er ikke noget meget stort problem. 
Det andet problem er at finde ud af om brøken er terminerende. Det er et noget større problem.

5 delt i 6 lige store dele

i <- 37
100^i/i^i

Det er givet, at for et givent N, befinder k sig i intervallet 1:(N-1)

maksk <- function(N){
  1:(N-1) %>% 
    as_tibble() %>% 
    #mutate(fraction = N^value/value^value) %>% 
    mutate(fraction = value*(log(N)- log(value))) %>% 
    arrange(desc(fraction)) %>% 
    head(1) %>% 
    .[,1] %>% 
    as.integer()
    
}

Det er her det går galt. Jeg finder ikke den maksimale.
Selvfølgelig gør jeg ikke det. For jeg løber tør for plads.
Så.

brøken er: N^k / k^k
eller (N/k)^k

B = (N/k)^k

ln(B) = k*ln(N/k)
ln(B) = k(ln(N) - ln(k) )

Det går stadig galt. 

The value of k that maximizes P as M(N) (or Pmax) can be calculated as N/e.

maksk(5)

i <- 9994
round(i/exp(1))

maksk(i)

5:10000 %>% 
  as_tibble() %>% 
  rowwise() %>% 
  mutate(diff = round(value/exp(1))-maksk(value)) %$%
  sum(diff)
  tail() 

  det tyder ret meget på at jeg beregner den maksimale k korrekt.

test <- c(9858, 3627)  



106/39
library(gmp)
gcd(9858, 3627)

red <- function(ratio){
  while(gcd(ratio[1], ratio[2])>1){
    x <- ratio[1]/gcd(ratio[1], ratio[2])
    y <- ratio[2]/gcd(ratio[1], ratio[2])
    ratio[1] <- x
    ratio[2] <- y
  } 
  str_c(ratio[1],"/",ratio[2])
}

test <- c(9860, 3627)


red(test)  

library(dplyr)
library(magrittr)
5:10000 %>% 
  as_tibble() %>% 
  rowwise() %>% 
  mutate(mk = maksk(value)) %>% 
  mutate(redfrac = red(c(value, mk))) %>% 
  mutate(naevner = if_else(str_detect(redfrac, "/"), as.integer(str_extract(redfrac, '(?<=/)\\d*')), as.integer(1))) %>% 
  mutate(ok= all(primeFactors(naevner) %in% c(1, 2,5))) %>% 
  mutate(D = if_else(ok, -value, value))  %$% 
  sum(D)


Den brøk bliver ikke reduceret korrekt...



dette skulle være svaret.  48861552


det får jeg ikke. Jeg får for lidt...
48751206
  
49524392

Nu får jeg for meget...
  
  452936
  441110
  
2438

161: 8798
8637

(8959-8637)/2


161/140

160/140

161/7

all
  primeFactors(4)
  

test <- "5/2"

str_extract(test, "(?<=/)\\d")

Det giver mig så en redfrac, der indeholder brøken. Den kan dog godt være blot et hel tal
Den brøk skal så opløftes til mk.
Men. Det ændrer ikke på primfaktorene i nævneren. De er de samme. der er blot fler af dem,

Så. Hvis jeg kan pille dem ud der blot er et heltal. Det er også dem hvor value %% mk er 0. Ikke?

  Så hvis der er en "/" i strengen, er vi udelukkende interesseret i nævneren. Hvis der ikke er nævneren 1. 
  
    6%%2

Nå. Hvordan finder vi så ud af om value^mk/mk^mk er terminating?
  Det lader til, at hvis vi reducerer brøken, kan vi nøjes med at kigge på nævneren. Hvis dens primtals faktorisering udelukkende 
indeholder 2-taller og 5-taller, så er brøken terminerende.

Det lader sig ikke gøre at beregne denominator direkte. Det største tal er mindst 78^78.

Så vi skal kunne håndtere:
  10000^78  / 78^78

eller: (100000/78)^78

library(numbers)
primeFactors(9999) %in% primeFactors(78)
library(MASS)
str(fractions(5/2))






