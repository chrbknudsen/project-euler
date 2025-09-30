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
  print(n)
  c <- as.bigz(as.numeric(a*n))
  if(c < min(coins)){
  coins <- append(coins, c)
  print("antal coins fundet: ")
    print(length(coins))
  }

}




sum(coins)
