104

Det handler om at finde de første 9 cifre, og de sidste 9 cifre i fibonacci tal.

library(numbers)
fibonacci(523)

det kan vi ikke...

Det er let nok at finde de sidste 9

last_nine_fib <- function(n){
  if(n==0){return(1) break()}
  if(n==1){return(1) break()}
  i <- 2
  fibn1 <- 1
  fibn2 <- 1
  while(i!=n){
    fib <- (fibn1+fibn2)%%1000000000
    i <- i + 1
    fibn1 <- fibn2
    fibn2 <- fib
  }
  return(fib)
  
}

last_nine_fib(541)

Det er også enkelt at teste om de er palindromiske

all(1:9 %in% unique(unlist(str_split(last_nine_fib(541),""))))

fundet <- F
i <- 0
while(!fundet){
    i <- i + 1
    fundet <- all(1:9 %in% unique(unlist(str_split(last_nine_fib(i),""))))
}

Og dermed også let nok at finde ud af at det første fibonacci tal der er 9-palindromisk er 541

Det er vanskeligere at finde de første 9 cifre.

Wikipedia fortæller, at vi kan finde det net fibonaccital ved at afrunde udtrykket
phi^n/sqrt(5)

Hvor phi er (1+sqrt(5))/2

Det bliver meget store tal. 

Fn = phi^n/sqrt(5) logaritmeres

log(Fn) = log(phi^n) - log(sqrt(5)) <->
log(Fn) = n*log(phi) - log(sqrt(5))

Lad os teste. Vi ved at n=2749 er 9-palindromisk på de første 9 cifre.

n <- 2749
phi <- (1+sqrt(5))/2
logrod5 <- log10(sqrt(5))

n*log10(phi) - logrod5

Det er så ti-tals logaritmen til F2749

De første cifre får vi så ved:

  options(scipen=999)
10^(n*log10(phi) - logrod5-574)*10000000000

Og vi ganger med noget mere end det er nødvendigt for at få tilstrækkelig præcision.



Vi er kun interesserede i de første 9:
  str_sub(10^(n*log10(phi) - logrod5-574)*10000000000,1,9)

Er de palindromiske?
  
all(1:9 %in% unique(unlist(str_split(str_sub(10^(n*log10(phi) - logrod5-574)*10000000000,1,9),""))))

lad os lave en funktion til at skaffe de første ni cifre for given n.


first_nine_fib <- function(n){
  str_sub(10^(n*log10(phi) - logrod5-floor(n*log10(phi) - logrod5))*10000000000,1,9)
}

Lad os også lave en funktion til at teste om de her tal er 9-palindromiske

palindromisk <- function(n){
  all(1:9 %in% unique(unlist(str_split(n,""))))
}


Nu er det så at starte fra en ende af, og teste om et givet n er palindromisk på de første 9 cifre.
Hvis det er, er det så også på de sidste ni?
  
fundet <- F  
i <- 0
while(!fundet){
  i <- i + 1
  if(palindromisk(first_nine_fib(i))){
    if(palindromisk(last_nine_fib(i))){
      fundet <- T
      return(i)
    }
  }
}
i
329 468