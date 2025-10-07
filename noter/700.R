700

en sekvens for n = 1 etc

1504170715041707n mod 4503599627370517

library(gmp)

a <- as.bigz("1504170715041707")
b <- as.bigz("4503599627370517")

modulus(a) <- b

c <- 1
n <- 0
coins <- as.bigz(1504170715041707)
while(c >= 0){
  n <- n + 1
  cat(sprintf("\rnået til %d", n))
  c <- as.bigz(as.numeric(a*n))
  if(c < min(coins)){
  coins <- append(coins, c)
  print("antal coins fundet: ")
    print(length(coins))
  print("seneste coin: ")
  print(c)
  print("seneste gæt:")
  print(sum(coins))
  }

}




sum(coins)
problemet er lidt at det kan kræve en del iteratoiner. Så der skal noget enklere til.


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

dermed kan vi finde n for en given eulercoin e.

n er stigende, og stopper når den giver en eulercoin = 1.
Så hvis 2 er en eulercoin, men n for 2 er større end n for eulercoin 1, så er den ikke
med - for den vil komme senere i sekvensen end 1.

Derfor kan vi starte fra 1 og gå opad. Vi skal hver gang tjekke at n for den fundne euler 
coin er mindre end den vi fandt lige før.



gcdex(b, a)


