56

digit_sum <- function(x){
  require(stringr)
  t <- str_split(x, "", simplify=TRUE)
  sum(as.integer(t))

}


t <- 1:12
library(dplyr)
library(tibble)
t %>% 
  enframe() %>% 
  mutate(power = 9^value) %>% 
  rowwise() %>% 
  mutate(digsum = digit_sum(power))




?enframe
digit_sum(8)
8^1 #8
8^2 #10
8^3 #8
8^4 #19
8^5 #26
8^6 #19
8^7 #26
8^8 #37
8^9 #35
8^10 #37
8^11 #62
8^12 #64

99^3

8+5+8+9+9+3+4+5+9+2

1+0+7+3+7+4+1+8+2+4


6+8+7+1+9+4+7+6+7+3+6
