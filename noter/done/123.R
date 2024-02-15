123


(pn - 1)^n + (pn+1)^n 

primtal <- Primes(10000000)


P <- function(n){
  primtal[n]
}


n <- 7037
P(n)


int2bin <- function(n){
  n <- rev(as.integer(intToBits(n)))
  n[which(n==1)[1]:length(n)]
}




Når vi opløfter et tal tal i en potens n, modulus m

operationen <- function(tal, n , m){
  potensen <- int2bin(n)
  moduler <- tal^1 %% m
  for(i in 2:length(rev(2^((length(potensen)-1):0) ) )){
    moduler[i] <- (moduler[i-1]*moduler[i-1]) %% m
  }
  moduler <- moduler*rev(potensen)
  moduler[moduler==0] <- 1
  moduler <- as.bigz(moduler)
  mod.bigz(prod(moduler), m) 
}


library(gmp)

sidste <- function(n){
  (operationen(P(n)-1, n, P(n)^2) + operationen(P(n)+1, n, P(n)^2)) %% P(n)^2
}


n <- 7036

operationen(P(n)-1, n, P(n)^2) + operationen(P(n)+1, n, P(n)^2)

2432886930
5046539521

3265970663
5049381481
P(n)^2

hm


n <- 7037

p <- as.bigz(P(n))

mod.bigz((p-1)^n + (p+1)^n, p^2)

library(gmp)
library(numbers)
primtal <- Primes(1000000)
for(n in 7037:length(primtal)){
  p <- as.bigz(primtal[n])
  if(mod.bigz((p-1)^n + (p+1)^n, p^2)  > 10^10){
    print(n)
    break()
  }
}
