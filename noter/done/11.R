111

library(numbers)

10^10


1000000000
9999999999


Ingen af dem har M mindre end 8. Og det kan ikke være 10. Så jeg skal for hver af dem have genereret alle
potentielle tal med 9 eller 8 gentagne 0:9



test <- 8888880000 + 0:9999
        123456789 
test <- 8888888 + 100:999*100000000

det finder ikke dem alle!

alle potentielle kandidater med 9 gentagne af n

n <- 1

test <- c(111111111*n *10 +0:9, 111111111 + (1:9)*10^9)


alle potentielle kandidater med 8 gentagne af n

n <- 1

test <- c(11111111*n *100 +0:99, 11111111 + (1:99)*10^8)



(11111111*n*10 + 0:9) + ((1:9)*10^9)


11111119

9911111111

test
test <- 1000000001:1000000009

res <- numeric()

for(i in 1:length(test)){
  if(isPrime(test[i])){
    res <- c(res, test[i])
  }
}

test <- 9000000001:9000000009

df

BxxxxxxxxA

base <- 11111111*9
test <- numeric()
for(i in 1:9){
  for(j in 1:9){
    test <- c(test, base*10+j+i*10^9)
  }
}

test

1234567890
res <- unique(res)


6 har en hvor der er 9 gentagelser

df <- data.frame(d=0:9, M=0, N=0, S=0)

df

df[10,2] <- 8
df[10,3] <- length(res)
df[10,4] <- sum(res)
sum(df$S)
