Leonhard Euler was born on 15 April 1707.

Consider the sequence 1504170715041707n mod 4503599627370517.

An element of this sequence is defined to be an Eulercoin if it is strictly smaller than all previously found Eulercoins.

For example, the first term is 1504170715041707 which is the first Eulercoin. The second term is 
3008341430083414 which is greater than 
1504170715041707 so is not an Eulercoin. 
However, the third term is 
8912517754604 which is small enough to be a new Eulercoin.

The sum of the first 2 Eulercoins is therefore 1513083232796311.

Find the sum of all Eulercoins.

Udfordringen er så at finde dem alle...


For example, the first term is 1504170715041707 which is the first Eulercoin. The second term is 
3008341430083414 which is greater than 
1504170715041707 so is not an Eulercoin. 
However, the third term is 
8912517754604 which is small enough to be a new Eulercoin.


1 1504170715041707
2 3008341430083414
3    8912517754604

library(gmp)
coin <- as.bigz(1504170715041707)
modu <- as.bigz(4503599627370517)

(coin*modu) %% modu

?isprime

factorize(coin)
17      1249    12043   5882353

isprime(modu)
Det er faktisk ikke super interessant at finde den hvor vi er nået til 0. Det er fint nok at vi finder den n der giver os en 
eulercoin lig 1. 
For den næste eulercoin der er mindre end 1, vil være 0.

Så tricket må være at finde det n, der er 
(n*coin) %% modu = 1




(coin*8) %% modu
modu %% coin

f <- function(x){
  (coin*x) %% modu
}
library(dplyr)
lapply(1:10000000, f) %>% 
  lapply(as.character) %>% 
  unlist() %>% 
  as.numeric() %>% 
  min()


Den multiplikative egenskab:
  (a x b) %% n = (a %% n x b %% n) %% n


Det bliver ikke mindre end 0.
Og det bliver det, når 
coin ganget med n er et helt multiplum af modu. Så. Om ikke før, så når:
  
  
  (coin*modu) %% modu


25 %% 5

test <- modu%%coin
modu%/%coin

(coin * test)%%modu
