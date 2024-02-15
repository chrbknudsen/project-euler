119

digit_sum <- function(x){
  x <- str_split(x,"")
  x <- map(x, as.integer)
  x <- map(x, sum)
  return(unlist(x))
}

any(digit_sum(512)^(1:3) == 512)


t <- 1:1010


any(digit_sum(t)^(1:10) == t)


matrix(unlist(map(t, digit_sum) ),ncol=10, byrow=T)


Der er jo et klart mønster i hvad tværsummen er.
Den starter med 1, tæller op til 10. så begynder den forfra med 2, og tæller op til 11. Fra 3 til 12, fra 4 til 13
fra 5 til 14


(1:28)^(1)

t <- 1:100

res <- numeric()
for(i in 1:10){
  res <- c(res, t^i)
}
options(scipen=999)

expand.grid(1:100, 1:10) %>% 
  rename(digitsum=Var1) %>% 
  rename(eksp = Var2) %>% 
  mutate(raised = digitsum^eksp) %>% 
  mutate(nydigitsum = map(raised, digit_sum)) %>% 
  filter(nydigitsum==digitsum) %>% 
  filter(raised>9) %>% 
  arrange(raised)

