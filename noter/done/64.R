64


library(tidyverse)


# 
# Nå. Men rækken for e er:
#   2; 1, 2, 1, 1, 4, 1, 1, 6
# 
# Hvordan genererer vi den?

library(gmp)  
x <- 4
sqrt_cont_frac <- function(x){
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
library(tidyverse)
1:10000 %>% 
  enframe() %>% 
  rowwise() %>% 
  mutate(period = length(sqrt_cont_frac(value))-1) %>% 
  ungroup() %>% 
  mutate(odd = period %% 2) %>% 
  summarise(resultat = sum(odd))


test <- c(2,3,5,6,7,8,10,11,12,13)
for(i in 1:length(test)){
  print(length(sqrt_cont_frac(test[i]))-1)
}
5%%2

library(tidyr)
?who
