Nim.

"https://en.wikipedia.org/wiki/Nim#Winning_positions"

# hvor mange n <= n^30
# X(n,2n,3n) = 0


# Hvis bunkerne 
# 
# hver bunke skal repræsenteres binært. Og så skal vi lave xor på dem.
# 
# xor(8,76
# Binary Logic

a <- 6
a <- 1:2**30
sapply(a, function(x) bitwXor(x,2*x))

bitwXor(bitwXor(a, 2*a),3*a)
bitwXor(2*a,3*a)

bitwXor(bitwXor(1,2),3)


Hvis den sum skal være 0.
Så skal sidste bit være:
  110
000
101
011


I og med at vi har en hvor vi ganger med 2 - og derfor altid har 0 i den midterste mulighed - 
  Så vil x kun kunne være 0, når både a og 3*a er ulige.
Men det hjælper os ikke.
For hvis a er ulige, er 3a også ulige.

library(dplyr)
1:2^5 %>% 
  enframe(name=NULL) %>%
  rename(n=value) %>% 
  mutate(n2 = 2*n) %>% 
  mutate(n3 = 3*n) %>% 
  mutate(nimsum = !bitwXor(bitwXor(n,n2),n3)) %>% 
  summarise(antal = sum(nimsum))

89-55

  View()
2^1  =      2  
2^2  =      3  
2^3  =      5  
2^4 =       8
2^5  =     13
2^6  =     21
2^7  =     34
2^8  =     55  
2^9  =     89
2^10 =    144  
2^11 =    233
2^12 =    377

2^20 =  17711
2^25 = 196418  
  
test <- 1:30 %>% 
  enframe(name=NULL) %>% 
  mutate(antal = NA)

test[1,2] <- 2
test[2,2] <- 3

for(i in 3:30){
  test[i,2] <- test[i-1,2] + test[i-2,2]
}
tail(test)

Det kunne godt se ud som om at mønsteret er:
  Hvis der er to 1-taller ved siden af hinanden i den binære repræsenation af n - så er nimsummen ikke 0

Dermed kunne man få den tanke, at hvis man generer alle binære repræsentationer af tal fra 1 til 2^30, der ikke 
har to 1-taller ved siden af hinanden - så har man alle de tal der har en nim-sum på 0.
Så hvordan gør man det?

log2(2)
log2(3)
log2(18)


library(tibble)
library(dplyr)

a <- 1:2**30
a %>% 
  as_tibble %>% 
  rename(a=value) %>% 
  mutate(a2 = 2*a, a3 = 3*a) %>% 
  mutate(res=bitwXor(bitwXor(a,a3),a2)) %>% 
  mutate(test = floor(log2(a3))>floor(log2(a2))) %>% 
  View()

a %>% 
  as_tibble %>% 
  filter(!(floor(log2(3*value))>floor(log2(2*value))))

Logaritme regneregler.
log(3*a)
hvad er det?
  
  det er log(3) + log(a) > log(2) + log(a)

Der skal rundes ned.
det vil sige, at 2,9999 bliver til 2.
2,5 - 2,5 er 0.
3 - 2.99999 er 1
library(purrr)
reduce

Reduce(f = "+", x = a, accumulate = FALSE)
reduce(a, `+`)
a <- NULL
Reduce(a, '+')
a <- map(a, ~x+2)

reduk <- function(a,b){
  
}

Den akumulerede værdi er a. Den værdi der tilføjes er b.


Det vi klart ser er, at 2^x altid giver 0.
  10   2
 100   4
 110   6

 Hvor mange cifre er der i den binære repræsentation af et tal?
   
log2(16)
 
 Så hvis log2(3*a) > log2(2*a) så er den i hvert fald ikke 0.

 floor(log2(3) + 1)
 
  
 
 5
 
 bitwXor(4,6)
 
  101    5
 1010   10
 1111   15
 
 Og det betyder at alle multipla af fem giver 0.
 
 

bitwXor(3,3)

!bitwXor(bitwXor(x,2*x),3*x)

2**x30
as.binary(20)

library(purrr)

library(dplyr)
?all_equal

#a er den accumulerede værdi, x er det næste argument


redfun <- function(a,x){
a <- a + !bitwXor(bitwXor(x,2*x),3*x)
}

test <- 1:2^30
reduce(test, redfun)  
