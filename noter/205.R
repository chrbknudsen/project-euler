# 205
# 
# Peter has nine four-sided (pyramidal) dice, each with faces numbered 1, 2, 3, 4.
# Colin has six six-sided (cubic) dice, each with faces numbered 1, 2, 3, 4, 5, 6.
# 
# Peter and Colin roll their dice and compare totals: the highest total wins. The result 
# is a draw if the totals are equal.
# 
# What is the probability that Pyramidal Pete beats Cubic Colin? Give your answer rounded to 
# seven decimal places in the form 0.abcdefg


library(tidyverse)
start <- Sys.time()
pyra <- 1:4
p_results <- expand.grid(pyra, pyra, pyra, 
                         pyra, pyra, pyra, 
                         pyra, pyra, pyra)

cubi <- 1:6
c_results <- expand.grid(cubi, cubi, cubi,
                         cubi, cubi, cubi)

c_results <- c_results %>% 
  mutate(summen = Var1 + Var2 + Var3 + Var4 + Var5 + Var6)

p_results <- p_results %>% 
  mutate(summen = Var1 + Var2 + Var3 +
                  Var4 + Var5 + Var6 +
                  Var7 + Var8 + Var9)

c_results <- c_results %>% 
  select(summen) %>% 
  count(summen)

p_results <- p_results %>% 
  select(summen) %>% 
  count(summen)

results <- c_results %>% 
  mutate(lige=NA,
         larger = NA,
         smaller = NA)



for(i in 1:nrow(results)){
  val <- as.integer(results[i,1])
  results[i,3] <- sum(p_results[p_results$summen == val,]$n)
  results[i,4] <- sum(p_results[p_results$summen > val,]$n)
  results[i,5] <- sum(p_results[p_results$summen < val,]$n)
}

results %>% 
  mutate(odds_lige = lige/262144,
         odds_tab = larger/262144,
         odds_vin = smaller/262144) %>% 
  mutate(lige = n * odds_lige,
         tab = n * odds_tab,
         vin = n * odds_vin) %>% 
  summarise(sum = sum(n), win = sum(vin), loss=sum(tab)) %>% 
  summarise(svar = loss/sum) %>% 
  as.numeric()

slut <- Sys.time()
slut - start
options(digits=8)

svar

as.numeric(svar[1,2])

Så. Når jeg slår en sekser, så vil jeg altid tabe.
Når jeg slår en 9'er er sandsynligheden for at det er lige 

val <- 10

sum(p_results[p_results$n == val,]$n)

p_results %>% 
  view()

c_results[12,1]

p_results[p_results$summen==17,]

c_test <- sample(c_results$summen, 100000000, replace=T)
p_test <- sample(p_results$summen, 100000000, replace=T)


data.frame(C = c_test, P = p_test) %>% 
  mutate(P = p_test > c_test) %>% 
  summarise(res = sum(P))

57320115    100000000
57316294    100000000
57314277

p_results %>% 
  ggplot(aes(summen)) +
  geom_histogram(binwidth=1)
