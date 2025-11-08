project euler 93

library(dplyr)

digits <- 0:9

sets of digits:

all_digits <- expand.grid(digits,digits,digits,digits)

operations <- c("+", "-", "*", "/")

all_operations <- expand.grid(operations,operations,operations, stringsAsFactors = F)

 a + b  + c + d
(a + b) + c + d
 a + b + (c + d)
(a + b + c) + d
a + b + c + d) 

Hvordan beskrive paranteserne?
  
  Der er følgende muligheder:
  Helt uden
  a+b+c+d
  en parantes omkring 2 tal
  (a+b)+c+d
  a+(b+c)+d
  a+b+(c+d)
  to paranteser om to tal:
  (a+b)+(c+d)  
  en parantes om tre tal:
  (a+b+c)+d
  a+(b+c+d)
  en parantes om tre tal og en om to tal
  ((a+b)+c)+d
  (a+(b+c))+d
  a+((b+c)+d)
  a+(b+(c+d))
  
  
p1 <- rep("",17)
p2 <- c("", "(", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "")

basepattern <- c(
  '',	'',	'a',	'O1',	'',	'',	'b',	'',	'O2',	'',	'c',	'',	'',	'O3',	'd',	'',	'',
  '',	'(',	'a',	'O1',	'',	'',	'b',	')',	'O2',	'',	'c',	'',	'',	'O3',	'd',	'',	'',
  '',	'',	'a',	'O1',	'',	'(',	'b',	'',	'O2',	'',	'c',	')',	'',	'O3',	'd',	'',	'',
  '',	'',	'a',	'O1',	'',	'',	'b',	'',	'O2',	'(',	'c',	'',	'',	'O3',	'd',	')',	'',
  '',	'(',	'a',	'O1',	'',	'',	'b',	')',	'O2',	'(',	'c',	'',	'',	'O3',	'd',	')',	'',
  '',	'(',	'a',	'O1',	'',	'',	'b',	'',	'O2',	'',	'c',	')',	'',	'O3',	'd',	'',	'',
  '',	'',	'a',	'O1',	'',	'(',	'b',	'',	'O2',	'',	'c',	'',	'',	'O3',	'd',	')',	'',
  '(',	'(',	'a',	'O1',	'',	'',	'b',	')',	'O2',	'',	'c',	')',	'',	'O3',	'd',	'',	'',
  '',	'(',	'a',	'O1',	'',	'(',	'b',	'',	'O2',	'',	'c',	')',	')',	'O3',	'd',	'',	'',
  '',	'',	'a',	'O1',	'(',	'(',	'b',	'',	'O2',	'',	'c',	')',	'',	'O3',	'd',	')',	'',
  '',	'',	'a',	'O1',	'',	'(',	'b',	'',	'O2',	'(',	'c',	'',	'',	'O3',	'd',	')',	')'
)

pattern <- matrix(basepattern, nrow=11, byrow=T)

parop <- list()




for(i in 1:64){
  pat <- pattern
  pat[pat == "O1"] <- all_operations[i,1]
  pat[pat == "O2"] <- all_operations[i,2]
  pat[pat == "O3"] <- all_operations[i,3]
  parop[[i]] <- as.data.frame(pat)

}

parop

library(data.table)
parop <- rbindlist(parop)

Lad os så lægge restriktionen a<b<c<d ind i all_digits
relevante_digits <- all_digits %>% 
  rename(a=Var1, b=Var2, c=Var3, d=Var4)

nrow(relevante_digits)





Og grundlæggende gentage:
parop <- as.matrix(parop)

udtryk <- parop %>% 
  as.data.frame() %>% 
  transmute(udtryk = paste0(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17))


  udtryk
  

library(purrr)

f <- function(x){
  parse(text=x)
}
udtryk <- udtryk %>% 
  transmute(noget = map(udtryk, f))


resultater <- function(a,b,c,d){
  a <- a
  b <- b
  c <- c
  d <- d
    res <- udtryk %>% 
    mutate(resultat = map(noget, eval))
  res
}

a <- 1
b<- 2
c<- 3
d<-4
(parse(text = udtryk[1,]))

resultater(1,2,3,5)


eval(parse(text = udtryk[1,]))

quote(udtryk[1,])

?transmute

parop
paropdig <- list()


par <- parop


for(i in 1:10000){
  par <- parop
  par[,3] <- relevante_digits[i,1]
  par[,7] <- relevante_digits[i,2]
  par[,11] <- relevante_digits[i,3]
  par[,15] <- relevante_digits[i,4]
  paropdig[[i]] <- as.data.frame(par)
}

muligheder <- rbindlist(paropdig)

muligheder %>% 
