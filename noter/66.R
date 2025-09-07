library(gmp)
library(tidyverse)

sqrt_cont_frac <- function(x){
  x <- as.integer(x)
  if(sqrt(x) %% 1 == 0){
    return(NA)
  }
  m_old = 0
  d_old = 1
  a_old = floor(sqrt(x))
  a_0 <- a_old
  res <- a_old
  a_new <- 0
  while(a_new != 2*a_0){
    m_new <- d_old*a_old - m_old
    d_new <- (x - m_new*m_new)/d_old
    a_new <- floor((a_0 + m_new)/d_new)
    res <- c(res,a_new)
    m_old <- m_new
    d_old <- d_new
    a_old <- a_new
  }
  return(res)
}

min_sol <- function(D){
  D <- as.bigz(D)
  done <- F
  test <- as.bigz(sqrt_cont_frac(D))
  h0 <- test[1]
  k0 <- as.bigz(1)

  if((h0^2 - D*k0^2) == 1){
    done <- T
    return(h0)
    exit()
  }
  
  h1 <- test[1]*test[2] + 1
  k1 <- test[2]
  if((h1^2 - D*k1^2) == 1){
    done <- T
    return(h1)
    exit()
  }
  test <- test[-1]
  i <- 2
  while(!done){
    if(length(test)<=i){
      test <- c(test,test)
    }
    h2 <-  test[i]*h1 + h0
    k2 <- test[i]*k1+k0
    if(as.logical((h2^2 - D*k2^2) == as.bigz(1))){
      done <- T
      return(h2)
      exit()
    }
    h0 <- h1
    k0 <- k1
    h1 <- h2
    k1 <- k2
    i <- i+1
  }
}

kand <- 1:1000 %>% 
  enframe() %>% 
  select(-name) %>% 
  filter(as.logical(sqrt(value)%%1)) %>% 
  .$value


kand
h <- numeric(length(kand))
h <- as.bigz(h)


length(kand)
length(h)
for(j in 1:length(kand)){
  h[j] <- min_sol(kand[j])
}
kand[which.max(h)]
