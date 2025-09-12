# Hint - diaphantine euler 108

Mindste værdi af n hvor 

$\frac{1}{x} + \frac{1}{y} = \frac{1}{n}$

har mere end 1000 distincte løsninger 

n = 4 har præcis 3 løsninger.


Så jeg har brug for at finde ud af hvor mange løsninger
der er for en given n.

Vi får et hint, der antyder at vi skal kigge på
diophantine løsninger.

så dem må vi researche

Det kan jo omskrives.

$n^2 = (x-n)(y-n)$

det vil sige at vi er på jagt efter faktorer af n^2.

library(numbers)
numbers::primeFactors(16)
Men det giver os primfaktorerne. Vi er ude efter alle faktorer.

library(gmp)
factorize(16)
divisors(16)
Så - løsninger for 
1,16 + 4
2,8 + 4
4,4 + 4
parvis - og er der en i midten - også den.

Et bud er derfor at det vi leder efter er et n
for hvilket divisors(n^2) > 2000



n <- 1
n_divisors <- 0
while(n_divisors < 2000){
  n_divisors <- length(divisors(n^2))
  n <- n + 1
}


Men er der en genvej? Hvordan finder vi lettest ud af hvor mange divisorer et tal har?

Hvis vi finder primfaktorerne, tæller hvor mange af hver der er - hvordan formulerer man det meningsfuldt?
  hvert af disse antal lægger man 1 til, og ganger dem sammen. Så får man antallet af divisorer i tallet.
Så hvis vi i stedet finder divisorerne i 2001.
  og ganger dem sammen - så får vi antallet af af divisorer 


prod(primeFactors(1000) + 1)


library(tidyverse)
primeFactors(2001) 
så er buddet at:

  as.bigz(2^30) * as.bigz(3^24) * as.bigz(5^3)
|> table() +1
Så 

divisors(5832)


vi ved at n=1260 giver 113 distincte løsninger.

prod(table(primeFactors(1260))  +1)
length(divisors(1260^2))
prod(primeFactors(1260^2) |> table() +1)

Så 
2^x + 3^y + 5^z etc med primtallene - 

  det tal vi får ud af det, vil have 
(x+1)*(y+1)*(z+1) ext

divisorer. Vi er nu ude efter den kombination af x,yz etc, der giver mere end 1000. Den laveste vel at mærke.

Og det er vitigere at x er lille end at y er - eller?

  
  
  vi er på jagt efter et kvadrattal med 2001 divisorer.

vi finder antallet af divisorer i et tal ved at finde primfaktorerne, og deres eksponenter.

library(numbers)
library(gmp)
primeFactors(525)
divisors(525) (12)


2*3*2

primeFactors(12)

2*3*5^2

length(divisors(150))
primeFactors(2001)
sqrt(mpfr((2^as.bigz(28))*(3^as.bigz(22))*(5^2)))
sqrt(mpfr((2^as.bigz(12))*(3^10)*(5^6)*(7)))
Rmpfr::`.__T__as.integer:base`
library(Rmpfr)
install.packages("Rmpfr")

det kan reduceres yderligere. n^2 = ab



Og den øvre grænse må være produktet af et antal primtal, i første potens.