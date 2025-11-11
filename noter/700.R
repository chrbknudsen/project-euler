700

en sekvens for n = 1 etc

1504170715041707n mod 4503599627370517

library(gmp)

a <- as.bigz("1504170715041707")
b <- as.bigz("4503599627370517")



library(numbers)

start med n = 1. det giver en coin.
fortsæt med n = 2. det giver også en coin, men det er kun en euler coin, hvis den er mindre end den sidste vi fandt. 

Iterer til vi har fundet 15 eulercoins. Gør det ved at lade n vokse, og tjekke om den coin vi finder er mindre end den
mindste vi indtil videre har fundet. 


library(gmp)

a <- as.bigz("1504170715041707")
b <- as.bigz("4503599627370517")
modulus(a) <- b
c <- 1
n <- 0

coins <- as.bigz(1504170715041707)
stop_n <- length(coins)
while(stop_n <= 2){
  n <- n + 1
  cat(sprintf("\rnået til %d", n))
  c <- as.bigz(as.numeric(a*n))
  if(c < min(coins)){
  coins <- append(coins, c)
  print("antal coins fundet: ")
  print(length(coins))
  print("seneste coin: ")
  print(c)
    stop_n <<- length(coins)
  }

}

det tager lang tid. Men hvis vi kompilerer koden...

de_første_k <- function(k){
  k <- k -1
a <- as.bigz("1504170715041707")
b <- as.bigz("4503599627370517")
modulus(a) <- b
c <- 1
n <- 0

coins <- as.bigz(1504170715041707)
stop_n <- length(coins)
while(stop_n <= k){
  n <- n + 1
  c <- as.bigz(as.numeric(a*n))
  if(c < min(coins)){
  coins <- append(coins, c)
  stop_n <- length(coins)
  }

}  
  return(coins)
}

first_k <- compiler::cmpfun(de_første_k)

tst <- first_k(16)
tst
save(tst, file = "coins.rda")
load("coins.rda")
tst
Nu starter vi så nede fra. den n vi er nået til for den mindste coin vi har fundet er_

111054189*x = 12263410 
              42298633
save(coins, file="coins.csv")

Så nu:
coin = 1
den er mindre end 
111054189

og 
1*x = 3451657199285664 

er den n der matcher. Og den er større end 12263410

2*x = 2399714771200811 
2 er også mindre end 111054189

Men den anden betingelse er at 
2399714771200811 
skal være mindre end 3451657199285664

det er den. Derfor er 2 også en euler coin

3*x
3 er også mindre end 111054189
Og 1347772343115958 er mindre end 
   3451657199285664
derfor er 3 en eulercoin.

4*x
4 er mindre end 111054189
Men det tilhørende n, 295829915031105 
er større end 1347772343115958
og derfor er 4 ikke en euler coin.
)
??mpz_invert

library(gmp)

load("coins.rda")
coins

det er det vi skal teste op til. 
testere <- c(1:15806432)

testere <- as.bigz(testere)
x <- gmp::inv.bigz(a,b)

deres respektive n
n <- testere*x
max(n)
tail(n,10)
k <- rev(n)

which_test <- as.bigz(k)






is_record <- logical(length(which_test))
max_so_far <- as.bigz(-1)

which_test <- as.bigz(which_test)

for (i in seq_along(which_test)) {
  if (which_test[i] > max_so_far) {
    is_record[i] <- TRUE
    max_so_far <- which_test[i]
  }
}

is_record <- rev(is_record)

which_test[is_record]

tail(cand)

invmod(a,b)

gmp::gcdex(a, b)

modulus(a) <- b



sum(coins)
problemet er lidt at det kan kræve en del iterationer. Så der skal noget enklere til.

kan vi gå den anden vej? Hvis vi kan finde den n der giver eulercoin 1, har vi fundet den mindste.

Så kan vi arbejde os op derfra. 

den får vi ved n = gmp::inv.bigz(a,b)


(as.bigz("3451657199285664")*a)
gcd(a,b)

det forlyder at man kan teste om et tal er en nøgle. Og det er jo interessant,
da mit foreløbige forsøg bringer rummet ret langt ned.

Men hvordan katten fungerer det?

Der er en multiplikativ invers, fordi 
gcd(a,b) er lig 1.

Og derfor er der et tal x, hvor ax modulus b er lig 1.

Hvis vi kan finde dette x.

a*n = e (modulus b)

x*a*n = e*x (modulus b)
1*n = e*x (modulus b)


x <- gmp::inv.bigz(a,b)

10*x
as.bigz("1347772343115958 ")*a

Men. 5 er en eulercoin. Men. den finder vi ved en given n. Er denne n større end, eller mindre end noget.

Den skal være større end den sidste vi fandt ved at starte fra toppen. Og mindre end den sidste vi fandt ved at gå fra bunden.

dermed kan vi finde n for en given eulercoin e.

n er stigende, og stopper når den giver en eulercoin = 1.
Så hvis 2 er en eulercoin, men n for 2 er større end n for eulercoin 1, så er den ikke
med - for den vil komme senere i sekvensen end 1.

Derfor kan vi starte fra 1 og gå opad. Vi skal hver gang tjekke at n for den fundne euler 
coin er mindre end den vi fandt lige før.


Vi finder x ved den udvidede euklidiske algoritme som løser
as + bt = gcd(a,b)
as + bt = 1
vi regner modulo b. Så
as = 1 (modulo b)
gcdex(a,b) giver gcd, s og t.

s modulo b giver så x

g <- gcdex(a, b)
x <- g[2] %% b
modulus(x) <- b
så. 1 er en eulercoin.
så er det tilhørende n:
modulus(x) <- b



n 
1*x 
2*x
3*x
4*x
e <- 15806432 



get_n <- function(e){
  as.bigz(as.numeric(x*e))

}

get_n(1:10000000)

