145

Some positive integers n have the property that the sum [ n + reverse(n) ] consists entirely of odd (decimal) digits. 
For instance, 36 + 63 = 99 and 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904 are 
reversible. Leading zeroes are not allowed in either n or reverse(n).

There are 120 reversible numbers below one-thousand.

How many reversible numbers are there below one-billion (10^9)?
  
første og sidste ciffer skal have forskellig paritet. Eller hvad det nu hedder.
Sidste ciffer må ikke være 0.


Og derfor er der ikke nogen en-cifrede tal der er reversible.

library(tidyverse)

815 + 518
825 + 528
835 + 538
845 + 548
865 + 568


1 2
1 4
1 6
1 8
2 1
2 3
2 5
2 7
2 9
3 2
3 4
3 6
3 8
4 1
4 3
4 5
4 7
4 9
5 2
5 4
5 6
5 8
6 1
6 3
6 5
6 7
6 9
7 2
7 4
7 6
7 8
8 1
8 3
8 5
8 7
8 9
9 2
9 4
9 6
9 8




10:999 %>% 
  enframe(name=NULL) %>% 
  filter(!(value%%10==0)) %>% 
  mutate(reverse = str_split(value, "")) %>% 
  rowwise() %>% 
  mutate(reverse =  paste0(rev(unlist(reverse)), collapse="")) %>% 
  ungroup() %>% 
  mutate(reverse = as.integer(reverse)) %>% 
  mutate(summen = value + reverse) %>% 
  mutate(splittet = str_split(summen, "")) %>% 
  rowwise() %>% 
  mutate(tjek = !any(as.integer(unlist(splittet)) %% 2==0)) %>% 
  ungroup() %>% 
  summarise(antal = sum(tjek))

10:10^9 %>% 
  enframe(name=NULL) %>% 
  filter(!(value%%10==0))


Hvis de to første cifre er noget bestemt, stiller det krav til hvad de to sidste kan være.

101256 

Hvis de to første er 10, skal det sidste være lige, og det næstsidste skal være ulige.
hvis de to første er 11, skal det sidste være lige, og det næstsidste skal være lige
Hvis de to første er 12, skal det sidste være lige, og det næstsidste skal være ulige


abcdef
fedcba

ghijkl

l = a+f %% 10
k = 




01