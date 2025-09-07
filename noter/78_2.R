

P <- c(1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42, 56, 77, 101, 135, 176, 231, 297, 385, 490, 627, 792, 1002, 1255, 1575, 1958)

a <- function(x){
  x[x<0] <- NA
  res <- P[x+1]
  res[is.na(res)] <- 0
  res
}

A <- function(n){
  res <- 0
  k <- 1:n
  res <- sum(((-1)^(k+1))*(a(n - 0.5*k*(3*k-1)) + a(n - 0.5*k*(3*k+1))))
  return(res%%1000000)
}
}

n <- 26
seneste <- 1
while((seneste %% 1000000)!=0 ){
  seneste <- A(n)
  P[n+1] <- seneste
  n <- n + 1
}


det sker ved index 55375. Og eftersom rækken af tal er 0-indexeret, skal der trækkes 1 fra.
Så svaret er 55374

Men det må kunne gøres hurtigere...
)
55374



