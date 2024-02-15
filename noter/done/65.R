65

library(tidyverse)

Nå. Men rækken for e er:
  2; 1, 2, 1, 1, 4, 1, 1, 6

Hvordan genererer vi den?
  
  library(gmp)  

a <- as.bigz(2)
ekonv <- data.frame()
ekonv[1,1] <- 2

ekonv[2,1] <- 1
ekonv[3,1] <- 2

tilf <- function(x){
  i <- nrow(x)
  x[i+1,1] <- 1
  x[i+2,1] <- 1
  x[i+3,1] <- x[i,1]+2
  return(x)
}

ekonv <- tilf(ekonv)
ekonv <- tilf(ekonv)
ekonv <- tilf(ekonv)
ekonv
ekonv
library(gmp)
ekonv[1,2] <- as.bigz(2)
ekonv[1,3] <- as.bigz(1)
ekonv[2,2] <- ekonv[1,1]*ekonv[2,1] + 1
ekonv[2,3] <- ekonv[2,1]


i <- 3
ekonv[i,2] <- ekonv[i,1]*ekonv[i-1,2] + ekonv[i-2,2]
ekonv[i,3] <- ekonv[i,1]*ekonv[i-1,3] + ekonv[i-2,3]

i <- i + 1
ekonv[i,2] <- ekonv[i,1]*ekonv[i-1,2] + ekonv[i-2,2]
ekonv[i,3] <- ekonv[i,1]*ekonv[i-1,3] + ekonv[i-2,3]

View(ekonv)
strsplit()
sum(as.integer(unlist(strsplit(as.character(ekonv[100,2]),""))))
