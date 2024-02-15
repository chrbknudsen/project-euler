# Some positive integers n have the property that the sum [ n + reverse(n) ] consists entirely of odd (decimal) digits. 
# For instance, 36 + 63 = 99 and 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904 are 
# reversible. Leading zeroes are not allowed in either n or reverse(n).
# 
# There are 120 reversible numbers below one-thousand.
# 
# How many reversible numbers are there below one-billion (10^9)?
#   
#   første og sidste ciffer skal have forskellig paritet. Eller hvad det nu hedder.
# Sidste ciffer må ikke være 0.

library(tidyverse)

reverse_digits_math <- function(number) {
  reversed = 0
  while (number > 0) {
    reversed = (reversed * 10) + (number %% 10)
    number = number %/% 10
  }
  return(reversed)
}



1:10
rev(1:10)

# Funktion til at finde det første ciffer af et tal
first_digit <- function(number){
  number %/% 10^signif(log10(number), digits = 0)
}

last_digit <- function(numbers) {
  numbers %% 10
}
2/542

library(tidyverse)
x <- 1:10^9
tests <- tibble(x=x)
tests <- tests %>% 
  mutate(last_digit = last_digit(x)) %>% 
  filter(last_digit != 0)

tests <- tests %>% 
  mutate(omvendt = map_int(x, reverse_digits_math)) %>% 
  mutate(sum = x + omvendt) %>% 
  filter(!str_detect(sum, "0|2|4|6|8"))


tests <- tests %>% 
  mutate(first_digit = first_digit(x))

tests <- tests %>% 
  mutate(first_plus_last = last_digit + first_digit)


tests <- tests %>% 
  mutate(paritet = first_plus_last %% 2)

tests <- tests %>% 
  filter(paritet != 0)

tests <- tests %>% 
  mutate(omvendt = map_int(x, reverse_digits_math))

test2 <- tests %>% transmute(y = x+omvendt)

test3 <- test2 %>% 
  filter(!str_detect(y, "0|2|4|6|8"))
nrow(tests)

test3 %>% sample_n(10)


str_detect("133557", "0|2|4|6|8")


str_detect(1234, "5")

test2 <- tests %>% 
  filter( first_digit(y) %% 2 == 1)

tests <- test2 %>% 
  filter( last_digit(y) %% 2 == 1)
  
tests <- tests %>% 
  mutate(y = as.character(y)) %>% 
  filter(!str_detect(y, "8"))


tests <- tests %>% 
  filter(!str_detect(y, "6"))

tests <- tests %>% 
  filter(!str_detect(y, "4"))

tests <- tests %>% 
  filter(!str_detect(y, "2"))

tests <- tests %>% 
  filter(!str_detect(y, "0"))
nrow(tests)

str_detect("133557", "0|2|4|6|8")


# og hvor hurtigt kan vi så bruteforce?

reverse_digits <- function(number) {
  # Konverterer tallet til en streng, splitter det i cifre, omvender rækkefølgen, og samler det igen
  reversed_str <- paste(rev(strsplit(as.character(number), "")[[1]]), collapse = "")
  # Konverterer den omvendte streng tilbage til et tal
  as.numeric(reversed_str)
}
revers <- function(z){sum(((z %/% 10^(0:trunc(log10(z)))) %% 10) * base::rev(10^(0:trunc(log10(z)))))}


reverse_digits(1234:4321)
revers(1234:4321)

revers()

library(microbenchmark)
x <- 987654321098
x <- 1234
microbenchmark(reverse_digits(x), revers(x), reverse_digits_math(x), times = 100)



reverse_digits_vectorized <- Vectorize(reverse_digits)
library(tidyverse)
tic <- Sys.time()
x <- 1:10^9
tests <- tibble(x=x)
tests %>% 
  filter(x %% 10 != 0) %>% 
  mutate(y = reverse_digits_vectorized(x)) %>% 
  transmute(x = as.character(x+y))  %>% 
  filter(!str_detect(x, "0|2|4|6|8")) %>% 
  nrow()
toc <- Sys.time()
toc-tic

bitwise!

revers <- function(z){sum(((z %/% 10^(0:floor(log10(z)))) %% 10) * base::rev(10^(0:floor(log10(z)))))}

tic <- Sys.time()
x <- 1:10^9
x <- x[x %% 10 != 0]
x <- x[x %/% 10^floor(log10(x)) %% 2 != (x %% 10) %% 2]

y <- sapply(x, revers)

z <- x + y

revers(1234)
qa <- tail(x)
qa[1]
revers(qa[1])
y <- purrr::map_int(x, revers)
y <- sapply(x, revers)

toc <- Sys.time()
toc-tic

first_digit <- function(number){
  number %/% 10^floor(log10(number))
}

number <- 123456789

sum(((number %/% 10^(0:floor(log10(number)))) %% 10) * base::rev(10^(0:floor(log10(number)))))

10^(0:floor(log10(number)))


z <- 1345
revers(z)
revers(qa[1])
floor(log10(qa[1]))
5 %% 2
floor(9)
log10(qa[1])
last_digit <- function(numbers) {
  numbers %% 10
}

floor(log10(999999999))+1

