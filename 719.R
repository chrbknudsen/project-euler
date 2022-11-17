library(tidyverse)
Limit for hvor mange vi skal tjekke

81 er et S-tal. Fordi 8+1 er lig med 9

options(scipen = 17)



sqrt(10^12)

1:10000
tag alle tal op til kvadratrod N.
Kvadrer dem. Kan der laves en sum af kvadratet, der er lig med kvadratrod N.

enframe(1:10) %>% 
  select(value) %>% 
  rename(rod = value) %>% 
  mutate(N = rod^2)

partitions::compositions(10)
1 ciffer. kan deles på en måde.
2 cifre kan deles på to måder
3 kan på : 1+1+1, 2+1 og 1+2
4 på 1+1+1+1, 2+2, 2+1+1, 1+2+1, 1+1+2, 3+1 1+3


as.binary(4)
install.packages("binaryLogic")
intToBits(12)
as.integer(paste(rev(as.integer(intToBits(3))), collapse=""))
library(R.utils)


fjern_nul <- function(x){
  x[x!=0]
}

partitioner <- function(x){
noget <- intToBin(0:x) %>% str_pad(x, side = "left", "0") %>% 
str_split("(?<=(.))(?!\\1)" ) %>% lapply(nchar)
noget <- noget[2:length(noget)]
noget %>% lapply(fjern_nul)
}


partitioner(4)[[2]] %>% 
  str_c("(.{",.,"})") %>% 
  str_c(collapse="") %>% 
  str_c("^",.,collapse = "")


lav_pattern <- function(x){
  x %>% str_c("(.{",.,"})") %>% 
    str_c(collapse="") %>% 
    str_c("^",.,collapse = "")
}
lapply(partitioner(4), lav_pattern)



str_match_all("8182", pattern="^(.{2})(.{1})(.{1})") %>% unlist() %>% .[-1] %>% 
  as.integer() %>% sum()

str_match_all("abcd", pattern=pattern) %>% unlist() %>% .[-1]


Når vi deler eksempelvis et firecifret tal op som beskrevet, kan vi gøre det
på følgende måder:
  
1+1+1+1
2 + 1 + 1
1+1+2
2 + 2
1+ 2 + 1
3 + 1
1 + 3
4

der er 8 i alt. Konceptet kaldes compositioner. 
https://en.wikipedia.org/wiki/Composition_(combinatorics)
Første trick, er at få beskrevet dem alle. 

Der er n^(n-1) kompositioner af n.

Det viser sig, at hvis man leder efter compositionerne af 4. Så kan man finde dem
ved at tælle, binært, fra 0 til 2^3
https://math.stackexchange.com/questions/31562/number-of-ordered-partitions-of-integer

Det ville være 
0000
0001
0010
0011
0100
osv 
Så tæller vi antallet af gentagne cifre:

0000  4
0001  3 1
0010  2 1 1
0011  2 2
0100  1 1 2 
osv. 
Og så har vi de ordnede partitioner af 4. 

Lad os lave en funktion, der returnerer kompositionerne af et givet tal:
  
kompositioner <- function(x){
  noget <- intToBin(0:(2^(x-1)-1)) %>% str_pad(x, side = "left", "0") %>% 
    str_split("(?<=(.))(?!\\1)" ) %>% lapply(nchar)
   noget <- noget[2:length(noget)]
   noget %>% lapply(fjern_nul)   # vi kan ikke rigtig bruge den hvor der ikke sker en opdeling til noget.
  } 




Vi kan nu generere en liste, med de 8 kompositioner af 4. Og nu er tricket at 
få splittet vores fire cifrede tal op i de enkelte dele, konvertere til 
tal, og lægge dem sammen. Her er et eksempel:

str_match_all("8182", pattern="^(.{2})(.{1})(.{1})") %>% unlist() %>% .[-1] %>% 
  as.integer() %>% sum()

Det er kompositionen 2 + 1 + 1 vi bruger her.
Det kan vi ikke skrive i hånden hver gang.

Så lad os skrive en funktion, der kan tage en komposition, og forvandle den 
til det regulære udtryk vi skal bruge.

lav_pattern <- function(x){
  x %>% str_c("(.{",.,"})") %>% 
    str_c(collapse="") %>% 
    str_c("^",.,collapse = "")
}

Bum.

Nu har vi værktøjerne.

Vi skal arbejde med tal op til 10^12


for at det skal gå godt:
  options(scipen = 17)

Det er kun kvadrattal vi skal arbejde med. 
Det vil sige at der tallene fra 1 til 1.000.000 vi er interesserede i.
Dem kvadrerer vi. Så splitter vi dem jf ovenstående i deres kompositionter.
Dem lægger vi sammen, og så ser vi om de er lig det oprindelige tal.

Lad os starte med tallene op til 10^4. Altså kvadraterne af tallene 1 til 100:

Tallene skal kunne deles op i mindst to dele. Så det er faktisk tallene fra
4 til 100 vi skal kigge på:
  
tibble(rod = 4:100) %>% 
  mutate(N=rod^2) %>% 
  mutate(n = nchar(N)) %>% 
  mutate(kompositioner = map(n, kompositioner))

Vi skal egentlig ikke bruge kompositionerne. Vi skal bruge de regulære udtryk
der matcher dem.

regexp <- function(x){
  kompositioner(x) %>% lapply(lav_pattern)
}

Det er der ingen grund til at gøre hver gang. Lad os lave noget at slå op i.

vi har brug for patterns af længde 2 til 13.
mønstre <- c(2:13) %>% lapply(regexp)

Vi kan nu trække mønstrene fra mønstre. er det eg kompositionerne af 3:
  mønstre[[3]]

tibble(rod = 4:100) %>% 
  mutate(N=rod^2) %>% 
  mutate(n = nchar(N)) %>% 
  rowwise() %>% 
  mutate(kompositioner = list(mønstre[[n-1]]))


