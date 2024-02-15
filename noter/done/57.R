# It is possible to show that the square root of two can be expressed as an infinite continued fraction.
# 
# √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
# 
# By expanding this for the first four iterations, we get:
# 
# 1 + 1/2 = 3/2 = 1.5
# 1 + 1/(2 + 1/2) = 7/5 = 1.4
# 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
# 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...  
# 
# 
# 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985

# In the first one-thousand expansions, how many fractions contain a 
# numerator with more digits than denominator?


1 + 1/x0
Nævneren
2
5
12
29
70
169
408
985



Tælleren er givet ved:
  xn = 2xn-1 + xn-2

det er nævneren også.

3
7
17    
41
99
239
577
1393

library(gmp)
n <- t <- as.bigz(1000)

n[1] <- 3
n[2] <- 7
n[3] <- 17

for(i in 4:1000){
  n[i] <- n[i-1]*2 + n[i-2]
}


t[1] <- 2
t[2] <- 5
t[3] <- 12

for(i in 4:1000){
  t[i] <- t[i-1]*2 + t[i-2]
}

b <- data.frame(t=t, n=n)

b %>% 
  mutate(lt = floor(log10(abs(t))) + 1) %>% 
  mutate(ln = floor(log10(abs(n))) + 1) %>% 
  filter(ln>lt)

Hm. det fungerer ikke at smide det ind i en dataframe. de store integers bliver konverteret
til doubles.



ln <- floor(log10(abs(n))) + 1
lt <- floor(log10(abs(t))) + 1

r <- data.frame(ln=ln, lt=lt)

r %>% 
  filter(ln>lt)

omend jeg vist har byttet om på noget. Men elles - her er resultatet.