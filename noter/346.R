# 346

I base 2 er 111 = 2^2+2^1+2^0

I base 6 er 7 lig med = 6^1 + 6^0 

2^2+2^1+2^0

Så repunits i base 2 under 10^12

I stedet for at tjekke alle tal under 10^12
genererer vi alle repunit tal i base 2,
alle repunit tal i base 3
etc deropad til vi har dem alle.

options(scipen=999)

En given base b giver som minimum 1 og b+1. Udtrykt som 1 og 11 i den givne base.

Første led i dem alle er 1. Andet er første led ganget med basen + 1. Tredie led er andet led ganget med basen + 1
Det gælder generelt.
Tilføjer jeg dem alle til en vektor - så skal jeg bare finde de tal der optræder mere end en gang.

OK. Vi starter med at lave en funktion der generere alle repunit tal for en given base b, under en given limit l

repunits <- function(b,l){
  næste <- 1
  res <- numeric()
  while(næste < l){
    res <- c(næste, res)
    næste <- res[1]*b+1
  }
  return(rev(res))
}

Hvorfor løber vi kun op til b = floor(sqrt(l) - 1)?
  


Anyway, vi tager alle baser op til sqrt(10^12) -1 = 10^6-1 = 999999
resultat <- unlist(lapply(2:999999, function(x) repunits(b=x, l=10^12)))

Alle tal større end 1000000 vil have to repunit tal: 1 og 11 i en arbitrær base. den næste vil være større end grænsen.

library(dplyr)

Så alle de repunit tal vi finder i base under 1000000, som er over 1000000 vil være

rest <- resultat[resultat>1000000]
rest <- rest[rest<10^12]


test <- c(resultat, rest)

sum(as.bigz(names(table(test)[table(test)>1])))



rigtig <- as.bigz(336108797689259276)
                  336108797689259276
                  336108797690259276
mit <-    as.bigz(336108797690259277)
rigtig - mit
library(gmp)
89259276 - 90259277
