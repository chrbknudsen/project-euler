76

Det er jo partition tingen. Eller coin partition, opgave 78. 
Det er partitionstallet for 100.

Så hvis jeg beregner de første 101 led (fordi rækken af partitionstal er 0-indekseret), har jeg det antal måder 100 kan deles op.

Da opgave 76 forudsætter at tallet skal skrives som summen af mindst to heltal, skal der trækkes en fra, da partitionen 100, falder
for det kriterium.

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
  return(res)
}


n <- 26
seneste <- 1
while(n<101 ){
  seneste <- A(n)
  P[n+1] <- seneste
  n <- n + 1
}

P[101]-1


190569291
190569291



