357


library(numbers)
library(tidyverse)
betragt divisorerne af n, kald dem d.
for alle d i n=30, gælder at d + n/d er primtal.

Fin summen af alle n under 100.000.000, for hvilke det gælder, at 
alle divisorer d af n, er d + n/d primtal.

1 er altid divisor i n. hvilket betyder at n+1 skal være et primtal.
Hvilket også betyder, at det her kun kan være gældende for lige n.
Så er vi nede på 50.000.000 der skal undersøges. 

Da n altid er lige, er 2 divisor. Det betyder, at 2 + n/2 skal være et primtal
Og derfor skal n/2 være ulige.

m*2 = n, m skal være ulige. så vi kan betragte ulige m mellem 1 og 50.000.000.

Så er vi nede på 25.000.000

m <- seq(1,50000000, by=2)
Mulige n:
m <- m*2
primtal <- Primes(100000000)
library(tidyverse)
kand <- m %>% 
  enframe(name=NULL) %>% 
  mutate(et = value+1) %>% 
  filter((et%in% primtal)) %>% 
  mutate(to = 2+value/2) %>% 
  filter((to %in% primtal))

rm(m)
bringer os ned på 458462 observationer

kand <- kand %>% 
  select(value)

divisors(99999838)

test <- slice(kand, 1:10)


# det her vil give resultatet. det tager bare en krig
resultat <- apply(kand, 1, function(x) all( (divisors(x) + x/divisors(x)  )    %in% primtal))



test <- as.integer(test$value)
test
test%%3

for(i in 3:11){
kand <- kand %>% 
  mutate(test = as.logical(value %% i)) %>% 
  mutate(tester = if_else(test, test, (i + value/i) %in% primtal)) %>% 
  filter(tester)
  }

for(i in 12:110){
  kand <- kand %>% 
    mutate(test = as.logical(value %% i)) %>% 
    mutate(tester = if_else(test, test, (i + value/i) %in% primtal)) %>% 
    filter(tester)
}

for(i in 111:1000){
  kand <- kand %>% 
    mutate(test = as.logical(value %% i)) %>% 
    mutate(tester = if_else(test, test, (i + value/i) %in% primtal)) %>% 
    filter(tester)
}

endeligekandidater <- kand %>% 
  select(value) %>% 
  rowwise() %>% 
  mutate(divisorer = list(divisors(value)))
  
library(purrr)       

divisors(30)
primate(30, list(divisors(30)))

y <- divisors(30)

x <- 30

y <- unlist(y)
primate <- function(x,y){
  y <- unlist(y)
  y <- y + x/y
  all(isPrime(y))
}

imaal <- kand %>% 
  select(value) %>% 
  mutate(divisorer = map(value, divisors)) %>% 
  mutate(primat = map2(value,divisorer, primate)) %>% 
  mutate(ok = unlist(primat)) %>% 
  filter(ok)

svaret <- imaal %>% 
  select(value)
options(scipen=999)
sum(svaret$value) + 1

det er fordi jeg glemmer 1.

sci.pen(999)
1739023853136- 1739023853137
       
# nedenstående kan bringe os ned på betydeligt færre muligheder.
kand %>% 
  mutate(tre = as.logical(value%% 3)) %>% 
  mutate(tredie = if_else(tre, tre, (3+value/3) %in% primtal)) %>% 
  filter(tredie) %>% 
  mutate(fire = as.logical(value%% 4)) %>% 
  mutate(fjerde = if_else(fire, fire, (4+value/4) %in% primtal)) %>% 
  filter(fjerde) %>% 
  mutate(fem = as.logical(value%% 5)) %>% 
  mutate(femte = if_else(fem, fem, (5+value/5) %in% primtal)) %>% 
  filter(femte) %>% 
  mutate(seks = as.logical(value%% 6)) %>% 
  mutate(sjette = if_else(seks, seks, (6+value/6) %in% primtal)) %>% 
  filter(sjette) %>% 
  mutate(syv = as.logical(value%% 7)) %>% 
  mutate(syvende = if_else(syv, syv, (7+value/7) %in% primtal)) %>% 
  filter(syvende) %>% 
  mutate(otte = as.logical(value%% 8)) %>% 
  mutate(ottende = if_else(otte, otte, (8+value/8) %in% primtal)) %>% 
  filter(ottende) %>% 
  mutate(elleve = as.logical(value%% 11)) %>% 
  mutate(elvte = if_else(elleve, elleve, (11+value/11) %in% primtal)) %>% 
  filter(elvte) %>% 
  mutate(ti = as.logical(value%% 10)) %>% 
  mutate(tiende = if_else(ti, ti, (10+value/10) %in% primtal)) %>% 
  filter(tiende) %>% 
  mutate(ni = as.logical(value%% 9)) %>% 
  mutate(niende = if_else(ni, ni, (9+value/9) %in% primtal)) %>% 
  filter(niende) %>% 
  select(value) %>% 
  mutate(divisorer = list(divisors(value)))

Når jeg har sortere fra 1 til 11 fra, så er der 125261 kandidater tilbage.



head(kand)

as.logical(30 %% 5 )

returnerer falsk. 

Jeg er ude efter at tage et tal. Hvis det ikke kan divideres med 5, skal sand returneres.
Hvis det kan divideres med 5

test <- kand %>% 
  select(-et, -to ) %>%
  rowwise() %>% 
  mutate(divisorer = list(divisors(value))) 




  mutate(hm = list(as.integer(unlist(divisorer)) + value/as.integer(unlist(divisorer))   ) )
  mutate(OK = all(as.integer(unlist(hm)) %in% primtal  ))


test
as.integer(test[6,1])
as.integer(unlist(test[6,2])) + as.integer(test[6,1])/as.integer(unlist(test[6,2]))
divisors()
