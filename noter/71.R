71

Forbløffende simpelt. Vi er kun ude efter brøker der er mindre end 3/7.

Vi er sådan set også kun ude efter den ene brøk for hver nævner, der er mindre end 3/7

Den finder vi ved:
floor(((3*d)/(7*d))*d))
sådan ca.


Umiddelbart før
3/7
n/d, where n and d are positive integers. If n<d,
og d <= 1000000

t <- 1000000

floor(((3*t)/(7*t))*t)

428571/t < 3/7
428572/t < 3/7

df <- data.frame(d=as.numeric(2:t))
library(dplyr)
library(purrr)
library(numbers)
df %>% 
  mutate(n = floor(((3*d)/(7*d))*d)) %>% 
  mutate(lige = (d%%2==0 & n %% 2==0))%>% 
  mutate(d = if_else(lige, d/2, d)) %>% 
  mutate(n = if_else(lige, n/2, n)) %>% 
  select(-lige) %>% 
  mutate(brøk = n/d) %>% 
  filter(brøk != 3/7) %>% 
  arrange(desc(brøk)) 

GCD(999997, 428570)
