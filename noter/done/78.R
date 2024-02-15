library(dplyr)
library(gmp)
p <- c(1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42, 56, 77, 101, 135, 176, 231, 297, 385, 490, 627, 792, 1002, 1255, 1575, 1958)

p <- as.bigz(p)


P <- p


a <- function(x){
  if(x >=0){return(P[x+1])}else(return(0))
  
}


A <- function(n){
  res <- as.bigz(0)
  for(k in 1:n){
    res <- res + as.bigz((-1)^(k+1))*(
      a(n - 0.5*k*(3*k-1)) + 
        a(n - 0.5*k*(3*k+1)))
  }
  return(res)
}

A(5)

n <- 5
res <- as.bigz("0")
for(k in 1:n){
  res <- res + as.bigz((-1)^(k+1))*(
    a(n - 0.5*k*(3*k-1)) + 
      a(n - 0.5*k*(3*k+1)))
}

n <- 5
res <- 0
k <- 1
as.bigz((-1)^(k+1))*(
  a(n - 0.5*k*(3*k-1)) + 
    a(n - 0.5*k*(3*k+1)))

(-1)^(2+1)
n - 0.5*k*(3*5-1)

a(-9)

n - 0.5*k*(3*5-1)
a(2)

k <- k+1


n <- 26
seneste <- 1
while((seneste %% 1000000)!=0 ){
  seneste <- A(n)
  P[n+1] <- seneste
  n <- n + 1
}




