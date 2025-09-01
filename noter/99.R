Comparing two numbers written in index form like 211 and 37 is not difficult, as any calculator would confirm that 211 = 2048 < 37 = 2187.

However, confirming that 632382^518061 > 519432^525806 would be much more difficult, as both numbers contain over three million digits.

Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K text file containing one thousand lines with a base/exponent pair on each line, determine which line number has the greatest numerical value.

NOTE: The first two lines in the file represent the numbers in the example given above.

log(a^x) = x*log(a)

518061*log(632382) > 525806*log(519432)

632382*log(518061)
filnavn <- "p099_base_exp.txt"
df <- readLines(filnavn)
View(df)

library(dplyr)
library(stringr)
library(magrittr)
library(tibble)
library(tidyr)
answer <- df %>% 
  enframe() %>% 
  separate(value, c("first", "second") ) %>% 
  mutate(res=as.numeric(second)*log(as.numeric(first))) %>% 
  arrange(desc(res)) %>% 
  slice(1) %>% 
  select(name) %>% 
  as.numeric()

answer

mutate(base )