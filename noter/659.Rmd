659


maks p finder vi vel hvor a = 1

det giver jf et eller andet herunder

p = 1+2*n

n^2 + k^2 /  1+2n   skal være et heltal

og 

(n+1)^2 + k^2 /  1+2n skal også være et heltal.



(6^2 + 3)/(1+2*6)
((6+1)^2 + 3)/ (1+2*6) 

(n+1)^2 + k^2 - (n^2 + k^2 )
1+2n


= 1

 1 + 2n 

1+2n



n^2 + k^2 
største fælles divisor med

(n+1)^2 + k^2

Forskellen på første og andet led må være et multiplum af primtallet p.

Så 

(n+1)^2 + k^2 - (n^2 + k^2)  = a*p

Hvert af de to led er også multipla af p:
  
  n^2 + k^2 =  b*p
  (n+1)^2 + k^2 = c*p  


Dermed også at:
  cp - bp = ap
Eller
  c-b = a <=>
  c = a + b  
  
  (n+1)^2 + k^2 = c*p  <=>
  n^2 + 1 + 2*n + k^2 = c*p
  
og:
  n^2 + 1 + 2*n + k^2 - (n^2 + k^2)  = a*p  <=>
  n^2 + 1 + 2*n + k^2 - n^2 - k^2  = a*p    <=>
  1 + 2*n  = a*p    

(n+1)^2 + k^2 = c*p  <=>
n^2 + 1 + 2*n + k^2 = c*p  <=>
n^2 + a*p + k^2 = (a+b)*p  <=>
n^2 + a*p + k^2 = a*p + b*p  <=>  
n^2 + k^2 = a*p + b*p - a*p  <=>
n^2 + k^2 = b*p  

Så hvad har vi

ap = 1+2*n
bp = n^2 + k^2

p bliver størst når a = 1 ?


bp = n^2 + k^2

(n + k)^2 
n^2 + k^2 + 2nk

bp = (n+k)^2 - 2nk

begge dividerbare med 13

Det er ret store primtal der potentielt skal findes...


(n+1)^2 - n^2 
n^2 + 1 + 2n - n^2
1+2n et primtal


Primes(100) - 1


k <- 10000000
options(scipen=999)
k^2 + 1^2 
k^2 + 2^2 
k^2 + 3^2 
k^2 + 4^2 



Førsteled er:
  
  n^2 + k^2 

andet led er 
 (n+1)^2 + k^2

de kan begge divideres med et primtal p

førsteled/p = a
andetled/p = b

førsteled/a = p
andetled/b = p

førsteled/a = andetled/b

førsteled = a* andetled / b

førsteled/andetled = a/b

a og b skal være heltal.

b skal være større end a

n^2 + k^2
(n+1)^2 + k^2

n^2 + k^2
n^2 + 1 + 2n + k^2


52-39

Når 39 er dividerbar med 13.
Så er det næste led lig med 39 + et eller andet multiplum af 13.
I dette tilfælde 1.

Det største primtal der kan komme på tale, er det primtal der er forskellen på det første led, og det andet.

f <- function(x){
  (1:20)^2 + x
}

test <- f(10)
test[-1] - test

forskellen på de enkelte successive led i rækken er uafhængig af k. Den afhænger kun af n.

Og n er blot rækken.

Så hvilke primtal optræder der i denne sekvens. For det er kandidaterne.
library(tidyverse)
test <- (2:121)^2 -(1:120)^2 

library(numbers)
test %>% 
  enframe(name=NULL) %>% 
  mutate(prim = unlist(map(value, isPrime))) %>% 
  View()





n^2 + k^2

for k = 1


n er sekvensens term.

k^2 er den vi summerer over.

k <- 1

n <- 1:10

library(dplyr)
library(numbers)
test <- n^2 + k^2 
test %>% 
  enframe(name=NULL)  %>% 
  mutate(divs = map(value, primeFactors)) %>% 
  unnest() %>% 
  View()

