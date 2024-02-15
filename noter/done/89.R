# 89
# 




# I = 1
# V = 5
# X = 10
# L = 50
# C = 100
# D = 500
# M = 1000
# 
# Numerals must be arranged in descending order of size.
# M, C, and X cannot be equalled or exceeded by smaller denominations.
# D, L, and V can each only appear once.
# 
# Only one I, X, and C can be used as the leading numeral in part of a subtractive pair.
# I can only be placed before V and X.
# X can only be placed before L and C.
# C can only be placed before D and M.
# 
# Hvornår er der en subtractive pair?
# 
# det er der, når I,C eller C starter, og 
# I efterfølges af V eller X
# X efterfølges af L eller C
# C efterfølges af D eller M

C_subtractive <- function(x){
  str_detect(x,"C.*[D|M]")
}

I_subtractive <- function(x){
  str_detect(x,"I.*[V|X]")
}


X_subtractive <- function(x){
  str_detect(x,"X.*[L|C]")
}

subtractive <- function(x){
  X_subtractive(x)|C_subtractive(x)|I_subtractive(x)
}

test <- "MMMCCCXLIV"

str_extract_all(test, "(C.*[D|M]|I.*[V|X]|X.*[L|C])(.*?)")
str_split(test, "(C.*[D|M]|I.*[V|X]|X.*[L|C])(.*?)")

tal <- function(x){
  res <- sum(værdier[unlist(str_split(unlist(str_split(x, "(C.*[D|M]|I.*[V|X]|X.*[L|C])(.*?)")),""))])
  for(i in unlist(str_extract_all(x, "(C.*[D|M]|I.*[V|X]|X.*[L|C])(.*?)"))){
    values <- værdier[rev(unlist(str_split(i, "")))]
    res <- res + values[1]-values[-1]
  }
  return(res)
}

tal(test)

Så er der ingen valide subtractive. Så splitter vi op, oversætter hver enkelt karakter til en talværdi og lægger sammen.

værdier <- c(1,5,10,50,100,500,1000)
names(værdier) <- c("I", "V", "X", "L", "C", "D", "M")

sum(værdier[unlist(str_split(test,""))])


# godt. Så kan vi klare de fleste:
read_lines("p089_roman.txt") %>% 
  enframe(name=NULL) %>% 
  mutate(subs = subtractive(value)) %>% 
  rowwise() %>% 
  mutate(dec = if_else(subs, tal(value), sum(værdier[unlist(str_split(value,""))]))) %>% 
  View()

Nu tror jeg faktisk jeg kan klare dem alle...
Det er så her jeg opdager at biblioteket "gtools" har en funktion roman2int, der kan klare det.

OK øvelse i regulære udtryk.

Nå. Men gtools har tilsyneladende ikke en tilsvarende funktion der kan konvertere til romertal.

der er en indbygget funktion as.roman, der kan klare alle tallene op til 3899

read_lines("p089_roman.txt") %>% 
  enframe(name=NULL) %>% 
  mutate(subs = subtractive(value)) %>% 
  rowwise() %>% 
  mutate(dec = if_else(subs, tal(value), sum(værdier[unlist(str_split(value,""))]))) %>% 
  mutate(kort = as.roman(dec)) %>% 
  filter(is.na(kort)) %>% 
  View()


Det klarer alle tallene, indtil de når 3905.
Det fine er, at der kan jeg så blot fjerne de to første Mer, konvertere, og tilføje dem igen.
Strengt taget behøver jeg ikke engang at gøre det. Jeg kan bare trække 2000 fra den decimale værdi, konvertere, finde
længden og lægge to til.


svar <- read_lines("p089_roman.txt") %>% 
  enframe(name=NULL) %>% 
  mutate(subs = subtractive(value)) %>% 
  rowwise() %>% 
  mutate(dec = if_else(subs, tal(value), sum(værdier[unlist(str_split(value,""))]))) %>% 
  mutate(kort = as.character(as.roman(dec))) %>% 
  mutate(L_opr = nchar(value)) %>% 
  mutate(kort = if_else(is.na(kort), paste0("MM",as.character(as.roman(dec-2000)), collapse=""), kort) ) %>% 
  mutate(L_ny = nchar(kort)) %>% 
  transmute(diff = L_opr - L_ny) %>% 
  ungroup() %>% 
  summarise(svar=sum(diff))
  

as.character(as.roman(3899))



library(gtools)



Hvis der er subtractive... 

test <- "MMMMDXCV"
X_subtractive(test)

str_split(test, "X*[L|C]")
str_extract_all(test, "X*[L|C]")



Så skal vi have pillet den del streng ud

str_split(test, "I*X")
str_extract_all(test, "(.*)([I*X])")
