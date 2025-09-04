source("solutions/helpers.R")
t_1000 <- c(0,triangle(1:44))
1000 - t_1000


t_mil <- triangle(0:5920472)
svar <- 0 
for(i in t_mil){
  for(j in t_mil){
    svar <- svar + ((17526000000000-i-j) %in% t_mil)
  }
}

svar
triangle(5920472) > 17526000000000

k> 17526000000000
n t_1 - t_2 - t_3 = 0

sum((1000 - t_1000) %in% rest)

library(tidyverse)
expand_grid(a=t_1000, b=t_1000, c = t_1000) |> 
  transmute(abc = a + b + c) |> 
  filter(abc == 1000)



expand_grid(a=0, b=t_mil, c = t_mil) |> 
  transmute(abc = a + b + c) |> 
  filter(abc == 1000000)

est
triangle
sqrt(17526)
