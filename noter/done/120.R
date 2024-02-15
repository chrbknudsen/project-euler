120




a <- as.bigz(7)

res <- ((a-1) + (a+1)) %% a^2
res <- as.bigz(res)

for(n in 2:1000){
  res[n] <- mod.bigz((a-1)^n + (a+1)^n, a^2)
}

max(res)


den_maximale <- function(a){
  a <- as.bigz(a)
    n <- 1:2000
  
    res <- mod.bigz((a-1)^n + (a+1)^n, a^2)

  
  max(res)
}



den_maximale(3)
svar <- 0
for(i in 3:1000){
  svar <- svar + den_maximale(i)
}

333082500 - svar
svar
