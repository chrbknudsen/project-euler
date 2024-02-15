358
Nå - men det var ikke vejen frem at forsøge at gange sig til det.

I stedet kan wikipedia fortælle hvordan man konstruerer cykliske tal.

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
