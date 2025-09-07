library(numbers)
Primes(1000)

# By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
# 
# By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten 
# generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this 
# family, is the smallest prime with this property.
# 
# Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime 
# value family.


Der er et mønster. 

Hvis tallet er på n cifre

Så er der: 2 i n-1 -1 mulige mønstre

n <- 5
2^(n-1)-1

Så genererer jeg samtlige binære tal fra 1-15 (decimalt), efter jeg har ganget med 2. Det giver et antal
mønstre

vi har et primtal 12347, som vi vil teste. De splitter vil til en vektor
t <- c(1, 2, 3, 4, 7)

for hvert mønster, inverterer vi mønsteret
eksempelvis:
  01100

intToBits(15)

ceiling(log2(15))

Så det bliver til 
m <- c(1, 0, 0, 1, 1)
t * m

Mønsteret, det oprindelige var:
  n <- c(0,1,1,0,0)

Det ganger vi med 4
4*n
og lægger til t*m

4*n + t*m

tobin <- function(x, n){
  as.integer(rev(intToBits(x)[1:n]))
}

revbin <- function(x,n){
  abs(as.integer(rev(intToBits(x)[1:n]))-1)
}


OK. Test tallet er 13
n <- 2 
n <- floor(log10(13))+1
t <- as.integer(unlist(str_split(as.character(13),"")))
2^(n-1)-1
1 muligt mønster.
Så jeg skal generere alle disse mønstre:
  library(numbers)
primtal[7]

56003


fundet <- 0
counter <- 11400
primtal <- Primes(999999)  

length(Primes(121313))
fundet
11415 er counterværdien når vi skal finde den med 8 erstatninger

Så den med 7 erstatninger, skal findes når vi når til counter 5684

while(fundet==0){
s <-  primtal[counter]

t <- as.integer(unlist(str_split(as.character(s),"")))

n <- length(t)

m <- (1:(2^(n-1)-1))*2  

tempo  <- integer()

for(i in 1:length(m)){
  p <- tobin(m[i], n)
  q <- revbin(m[i],n)
  for(j in 0:9){
    temp <- (j*p+q*t)
    temp <- as.integer(paste(as.character(temp), collapse=""))
    tempo[j+1] <- temp
  }
  if(sum(isPrime(tempo))==8 & sum(nchar(tempo) == 6)>=8){
    fundet <- s
  }
}
counter <- counter + 1
}




kollapser til integer, og tester for primalitet. Nu ganger vi ikke kun med 4, vi ganger med 0:9. Og så er vi 
interesseret i hvor mange af de her regnestykker, der ender med et primtal.

10000
01000
00100
00010

11000
10100
10010
01100
01010
00110

11100
10110
11010

11110

her er der fem cifre. Det giver 14 kombinationer. Hvorfor finder jeg kun 14? Jeg tæller basalt set binært fra 
1 til 1111 


2**4

1
10
11
100
101
110
111
1000
1001
1010
1011
1100
1101
1110
1111


Hvis det er på 6 cifre, er der andre, og flere kombinationer.

Jeg får brug for at kunne generere disse kombinationer.

Så får jeg brug for at kunne teste om disse kombinationer, hvor der er udskiftet på diverse positioner, er primtal.