Cyclic Numbers
Problem 358

library(tidyverse)
cyklisk tal.

Hvordan tjekker jeg at 
142857 og 285714 er "samme" tal?
  
  test <- "142857"
str_split_1(test, "")


Vi ved også at 137 skal kunne ganges med noget der giver 56789
som de første cifre.

for et af tallene 1 til n, ganget med noget der starter med 137
skal kunne give 56789

noget <- 1370 + 0:9

567898/noget

sum(((300000:500000)*137)%/%1000 == 56789)
411 til 414

411 %/% 10


eftersom det er cyklisk, så skal et af tallene starte med
137 og slutte emd 8 0'er

137  ... 00000000

Så et af tallene der skal ganges med er 100000000'

100000000

vi skal også have tal der ender med:
  
  56789000000001
567890000000013
5678900000000137

og som starter med:
  900000000137
8900000000137
78900000000137
678900000000137
5678900000000137

Det giver os den minimale længde n.



og mine mere oprindelige noter




358
Nå - men det var ikke vejen frem at forsøge at gange sig til det.

I stedet kan wikipedia fortælle hvordan man konstruerer
cykliske tal.
https://en.wikipedia.org/wiki/Cyclic_number
n er et cyklisk tal, hvis t = p - 1 i nedenstående

p må ikke dividere 10. Det er ikke noget stort problem

iris %>% mutate_if(is.double, as.integer)




library(gmp)
p <- as.bigz(17)
b <- as.bigz(10)
t <- as.bigz(0)
r <- as.bigz(1)
n <- as.bigz(0)

t <- t+1
x <- r*b
d <- x %/% p
r <- mod.bigz(x, p)
n <- n*b+d

library(stringr)

library(numbers)
library(dplyr)
library(tibble)
kand <- Primes(724637681,729927007)
              
primtallet skal ende på 09891
729927007
    09891

    
library(purrr)
library(dplyr)
library(numbers)    
library(tibble)
library(stringr)
    
kand <- 1:70000 %>% 
  enframe(name=NULL) %>% 
  mutate(first = str_starts(value, "7")) %>% 
  filter(first) %>% 
  transmute(kand=paste0(value,"09891")) %>% 
  transmute(kand = as.numeric(kand)) %>% 
  mutate(prim=map(kand, isPrime)) %>% 
  mutate(prim=unlist(prim)) %>% 
  filter(prim)

kand <- kand$kand


    
library(stringr)

library(gmp)
for(p in kand){
  test <- as.bigz(p)
  test <- as.bigz(10^(test-1)/test)
  test <- as.character(test)
  if(str_detect(test, "56789$")& str_detect(test, "^137")){
    print(p)
    break()
  }
}

Godt så har vi et bud på hvilket primtal der giver resultatet.
p <- 72509891
options(scipen=999)
1/p

    724637681
Selvom det giver et tal der starter og slutter som det skal.
Er det bare ikke svaret...

Der er for få nuller.

Så hvordan finder jeg ud af at det ikke er svaret?
  Det er ikke overraskende, at det starter og slutter med det korrekte. Hvorfor er det så ikke svaret?

p <- as.bigz(p)
test <- as.bigz(10^(p-1)/p)

p*test

test <- as.character(test)
Og så har vi et utroligt stort tal. Som en tekststreng.

noget <- test

sum(as.numeric(unlist(str_split(noget, ""))))




str_detect(test, "85567$")



Nå. Men man konstruerer cykliske tal ved:
  
  Lad b være tal basen. 10 i dete tilfælde.
Lad p være et primtal der ikke divider b. Så. Det skal blot
være et primtal der er større end 10.
sæt:
  t  = 0
r = 1
n = 0

Kør loop:
  t = t + 1
x = r*b
d = int(x/p)
r = x mod p
n = n*b+d
hvis r forskellig fra 1 gentag.

Hvis t = p - 1 er n et cyklisk tal.

Men bemærk at ikke alle værdier af p (primtallet) giver et 
cyklisk tal.

der skal arbejdes med store tal. så. gmp er det vist.

et cyklisk tal med længden L er den digitale repræsentation af 
1/(L + 1)

sÅ. 1/(6+1)

hm...

142857 genereres af primtallet 7.
Cykliske tal er karakteriseret ved at tallet ganget med dets
genererende primtal giver en sekvens af 9-taller (base - 1, og base er her 10.)

library(gmp)
L <- 100000000
L <- as.bigq(L,1)
1/(L+1)
format(1/(L+1), digits = 50)

as.bi

Alle cykliske tal er dividerbare med b-1 , altså i vores tilfælde 9.

så det tal vi leder efter er et multiplum af 9.
install.packages("Rmpfr")
library(Rmpfr)
L <- mpfr(729001101, precBits = 4000000)
x <- 1/(L+1)*100000000
formatMpfr(x, digits = 500000000)
                       729001101

1/729

L <- mpfr(799949, precBits = 10000)
x <- 1/L
formatMpfr(x , digits = 30)
library(numbers)

Primes(800000) %>% tail()
