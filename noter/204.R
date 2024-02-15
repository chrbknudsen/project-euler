204

tal der ikke har primfaktorer større end 100, og som er under 10^9

library(numbers)

primtal <- Primes(100)
l <- 10^9
Hvad er grænsen for et givet primtal?

floor(log(10^9)/log(primtal))

29*18*12*10*8*8*7*7*6*6*6*5*5*5*5*5*5*5*4*4*4*4*4*4*4

eksponenter <- function(x,r){
  0:floor(log(r)/log(x))
}
eksponenter(primtal[25],l)

l - primtal[25]^4

hov. 7^0 fører ikke til at 7 er primfaktor.

de skal ikke lægges sammmen! De skal ganges!!
  
l <- 10^8

l - primtal[25]^4

svar <- numeric()

for(i in eksponenter(primtal[25],l)){
  prod <- 1
  prod <- prod*primtal[25]^i
  rest <- floor(l/prod)
  for(j in eksponenter(primtal[24], rest)){
    prod <- prod*primtal[24]^j
    rest <- floor(l/prod)
    for(k in eksponenter(primtal[23], rest)){
      prod <- prod*primtal[23]^k
      rest <- floor(l/prod)
      
      if(prod <= l){svar <- c(svar,prod)}
      
    }
     }
  
}

l - primtal[25]^0*primtal[24]^1

eksponenter(primtal[23], l - primtal[25]^0*primtal[24]^1)

df <- data.frame()

primtal[25]

tal <- numeric()
for(i in primtal){
  tal <- c(tal, i^eksponenter(i,l))
}

library(dplyr)

sort(unique(tal)) %>% 
  enframe(name=NULL) %>% 
  arrange(desc(value))

l/(282475249*3)



primtal
library(tidyverse)
expand.grid(0:4, 0:4, 0:4, 0:4, 0:4, 0:4, 0:5) %>% 
  rowwise() %>% 
  mutate(produkt = prod(97^Var1, 89^Var2, 83^Var3, 79^Var4, 71^Var5, 67^Var6, 61^Var7)) %>% 
  filter(produkt <= l) %>% 
  mutate(Var8 = list(0:5)) %>% 
  unnest(Var8) %>% 
  mutate(produkt = produkt*59^Var8) %>% 
  filter(produkt <= l) %>% 
  mutate(Var9 = list(0:5)) %>% 
  unnest(Var9) %>% 
  mutate(produkt = produkt*53^Var9) %>% 
  filter(produkt <= l)

primtal
eksponenter(53,l)

l <- 10^9

primtal

expand.grid(eksponenter(2 ,l), eksponenter(3,l)) %>% 
  as_tibble() %>% 
  rename(V2 = Var1, V3 = Var2) %>% 
  mutate(produkt = 2^V2*3^V3) %>% 
  filter(produkt <= l) %>% 

  mutate(V5 = list(eksponenter(5,l))) %>% 
  unnest(V5) %>% 
  mutate(produkt = produkt*5^V5) %>% 
  filter(produkt <= l) %>% 

  mutate(V7 = list(eksponenter(7,l))) %>% 
  unnest(V7) %>% 
  mutate(produkt = produkt*7^V7) %>% 
  filter(produkt <= l) %>% 
 
  mutate(V11 = list(eksponenter(11,l))) %>% 
  unnest(V11) %>% 
  mutate(produkt = produkt*11^V11) %>% 
  filter(produkt <= l) %>% 

  mutate(V13 = list(eksponenter(13,l))) %>% 
  unnest(V13) %>% 
  mutate(produkt = produkt*13^V13) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V17 = list(eksponenter(17,l))) %>% 
  unnest(V17) %>% 
  mutate(produkt = produkt*17^V17) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V19 = list(eksponenter(19,l))) %>% 
  unnest(V19) %>% 
  mutate(produkt = produkt*19^V19) %>% 
  filter(produkt <= l) %>% 

  mutate(V23 = list(eksponenter(23,l))) %>% 
  unnest(V23) %>% 
  mutate(produkt = produkt*23^V23) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V29 = list(eksponenter(29,l))) %>% 
  unnest(V29) %>% 
  mutate(produkt = produkt*29^V29) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V31 = list(eksponenter(31,l))) %>% 
  unnest(V31) %>% 
  mutate(produkt = produkt*31^V31) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V37 = list(eksponenter(37,l))) %>% 
  unnest(V37) %>% 
  mutate(produkt = produkt*37^V37) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V41 = list(eksponenter(41,l))) %>% 
  unnest(V41) %>% 
  mutate(produkt = produkt*41^V41) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V43 = list(eksponenter(43,l))) %>% 
  unnest(V43) %>% 
  mutate(produkt = produkt*43^V43) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V47 = list(eksponenter(47,l))) %>% 
  unnest(V47) %>% 
  mutate(produkt = produkt*47^V47) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V53 = list(eksponenter(53,l))) %>% 
  unnest(V53) %>% 
  mutate(produkt = produkt*53^V53) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V59 = list(eksponenter(59,l))) %>% 
  unnest(V59) %>% 
  mutate(produkt = produkt*59^V59) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V61 = list(eksponenter(61,l))) %>% 
  unnest(V61) %>% 
  mutate(produkt = produkt*61^V61) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V67 = list(eksponenter(67,l))) %>% 
  unnest(V67) %>% 
  mutate(produkt = produkt*67^V67) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V71 = list(eksponenter(71,l))) %>% 
  unnest(V71) %>% 
  mutate(produkt = produkt*71^V71) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V73 = list(eksponenter(73,l))) %>% 
  unnest(V73) %>% 
  mutate(produkt = produkt*73^V73) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V79 = list(eksponenter(79,l))) %>% 
  unnest(V79) %>% 
  mutate(produkt = produkt*79^V79) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V83 = list(eksponenter(83,l))) %>% 
  unnest(V83) %>% 
  mutate(produkt = produkt*83^V83) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V89 = list(eksponenter(89,l))) %>% 
  unnest(V89) %>% 
  mutate(produkt = produkt*89^V89) %>% 
  filter(produkt <= l) %>% 
  
  mutate(V97 = list(eksponenter(97,l))) %>% 
  unnest(V97) %>% 
  mutate(produkt = produkt*97^V97) %>% 
  filter(produkt <= l) %>% 
  select(produkt) %>% 
  distinct()

primeFactors(88529281)
