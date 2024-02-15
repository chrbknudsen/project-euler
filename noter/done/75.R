# 75
# Alle primitive pythagoræiske tripler kan konstueres ved
# a = m^2 - n^2
# b = 2mn
# c= m^2 + n^2
# hvor m>n>0
# Begrænsningen fra opgaven giver os:
# a+b+c < 1500000
# m^2 + 2mn + m^2 < 1500000
# 2m^2 + 2mn < 1500000
# 2(m^2 + mn) < 1500000
# (m^2 + mn) < 1500000/2
# 
# Da m skal være større end n og n skal være større end 0, kan m højest være:
# 865
# m^2 + m < 1500000/2  
# 
# Yderligere begrænsninger. m og n må ikke begge være ulige. Der skal altså være mindst en af de to der er lige.
# Og så skal de være coprime.
# 
# 
# Alle kombinationer af m og n.
# 
# Filtrer dem fra hvor de ikke begge er ulige.
# 
# Filtrer yderligere de fra hvor de ikke er coprime
# 
# Så har vi alle primitive pythagoræiske tripler. Eller i hvert fald de m og n der skal bruges til at beregne dem.
library(tidyr)

library(numbers)
library(dplyr)
library(purrr)

expand.grid(m= 1:866, n = 1:866) %>%
  filter(m>n) %>%
  mutate(mindst_en_lige = 1-m%%2 + 1- n%%2) %>%
  filter(mindst_en_lige != 0) %>% 
  select(-mindst_en_lige)

Godt. er de coprime?
  
kandidater <- expand.grid(m= 1:866, n = 1:866) %>%
  filter(m>n) %>%
  mutate(mindst_en_lige = 1-m%%2 + 1- n%%2) %>%
  filter(mindst_en_lige != 0) %>% 
  select(-mindst_en_lige) %>% 
  mutate(koprim = map2(m,n, coprime))

kandidater_abc <- kandidater %>% 
  filter(koprim==T) %>% 
  select(-koprim) %>% 
  mutate(a= m^2 - n^2,
         b = 2*m*n,
         c = m^2 + n^2,
         total = a+b+c) %>% 
  filter(total <= 1500000)  

flere_kandidater <- kandidater_abc %>% 
  mutate(længder = map(total, function(x) seq(x, 1500000,x)))

test <- flere_kandidater[1:3,]

flere_kandidater %>% 
  unnest() %>% 
  select(a,b,c, længder) %>% 
  group_by(længder) %>% 
  add_tally()  %>% 
  ungroup() %>% 
  filter(n==1)
  
??unnest

længder[1:10] %>% 
  map(function(x) seq(x, 1500000,x))

sort(desc(længder))


1:10 %>%
  map(function(x) rnorm(10, x))


    
Den første har en længde på 12.
Det betyder, at vi kan danne wires med længden 12, 24, 36, 48 osv derop ad, indtil vi rammer 1500000
Den næste har længden 40. Så der kan dannes vires med længderne 40, 80, 120, 160 etc indtil vi rammer 1500000

Så for den første, på 12, kan vi danne følgende wirelængder:
seq(12,1500000, by=12)[1:10] %in%   seq(40,1500000, 40)
og for den næste, på 40, kan følgende dannes:
  


Den første på 12, har den første længde, der optræder i den andens efterfølgere, efter 10.

det er den vi får, når vi har 30,40,50
Og som altså har længden
120

Det er hvad vi opnår ved at gange den anden tripple med 3

Og det er en anden trekant end den første.



1500000/40


a = m^2 - n^2
b = 2mn
c= m^2 + n^2

xor(T,F)
