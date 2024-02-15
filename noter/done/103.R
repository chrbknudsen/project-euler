103

library(sets)

test <- c(6, 9, 11, 12, 13)
set_test <- set_power(test)


betingelse 1
S(B) ≠ S(C); that is, sums of subsets cannot be equal.




Så når jeg har lavet et sæt

test
testw <- c(20, 28, 35, 36, 37, 39, 42)
testr <- c(20,31,38,39,40,42,45)

test <- testw

cond1 <- function(test){
result <- T
set_test <- set_power(test)
for(i in set_test){
  if(set_is_proper_subset(i,test) & !set_is_empty(i) & result){
    set_test2 <- set_complement(i,test)
    set_test2 <- set_power(set_test2)
    for(j in set_test2){
      if(set_is_proper_subset(j,test) & !set_is_empty(j)){
       if(sum(i)==sum(j)){
         result <- F
         break()
         }
       }
     }
   }
}
return(result)
}



class(set_test)
set_cartesian()
Så er der betingelse 2. 

If B contains more elements than C then S(B) > S(C)

cond2 <- function(test){
  result <- T
  set_test <- set_power(test)
  for(i in set_test){
    if(set_is_proper_subset(i,test) & !set_is_empty(i) & result){
      set_test2 <- set_complement(i,test)
      set_test2 <- set_power(set_test2)
      for(j in set_test2){
        if(set_is_proper_subset(j,test) & !set_is_empty(j)){
          
          if(length(i)>length(j) & sum(i)<sum(j)){
            result <- F
            break()
          }
          if(length(j)>length(i) & sum(j)<sum(i)){
            result <- F
            break()
          }
          
          
        }
      }
    }
  }
  return(result)
}


cond12 <- function(test){
  result <- T
  set_test <- set_power(test)
  for(i in set_test){
    if(set_is_proper_subset(i,test) & !set_is_empty(i) & result){
      set_test2 <- set_complement(i,test)
      set_test2 <- set_power(set_test2)
      for(j in set_test2){
        if(set_is_proper_subset(j,test) & !set_is_empty(j)){
          if(sum(i)==sum(j)){
            result <- F
            break()
          }
          if(length(i)>length(j) & sum(i)<sum(j)){
            result <- F
            break()
          }
          if(length(j)>length(i) & sum(j)<sum(i)){
            result <- F
            break()
          }
          
          
        }
      }
    }
  }
  return(result)
}

paste(test,collapse="")
cond2(testr)

Vi får oplyst at vi med den beskrevne algoritme, kommer til et "near optimum set"

Eftersom det optimale n=6 sæt er
11,18,19,20,22,25
og måden at lave det near optimum sæt er:

c(19, 11+19, 18+19, 19+19, 20+19, 22+19, 25+19)  

Så skal vi nok være et par stykker på hver side af det sæt.

kand <- expand.grid(19:21, 29:31, 36:38, 37:39, 38:40, 40:42, 43:45) 

kand <- kand[kand$Var1 !=kand$Var4,]
kand <- kand[kand$Var3 !=kand$Var4,]
kand <- kand[kand$Var3 !=kand$Var5,]
kand <- kand[kand$Var4 !=kand$Var5,]
kand <- kand[kand$Var4 !=kand$Var6,]
kand <- kand[kand$Var5 !=kand$Var6,]
kand <- kand[kand$Var6 !=kand$Var7,]


Så beregner vi summen
kand$sum <- kand$Var1 + kand$Var2 + kand$Var3+ kand$Var4+ kand$Var5+ kand$Var6+ kand$Var7

og sorterer
kand <- kand[order(kand$sum),]
kand <- kand[,1:7]

for(i in 1:nrow(kand)){
  test <- kand[i,]
  if(cond12(test)){
    return(test)
    break()
  }
}


weird <- c(20,   28,   35,   36,   37,   39,   42)

set_power(weird)
for(i in set_power(weird)){
  print(i)
  
  print(sum(i)) 
  print(set_complement(i,weird))
  print(sum(set_complement(i,weird)))
  print("=============")
}


sum(tst)

for(i in 1:nrow(kand)){
  test <- kand[i,1:7]
  if(cond2(test)){kand[i,9] <- T}else{kand[i,9]<-F}
}

View(kand)

kand[1,1:7]

sum(c(21,	32,	39,	40,	41,	43,	46))
sum(c(20, 31, 38, 39, 40, 42, 45))
    c(20, 28, 35, 36, 37, 39, 42)
    
    Ah! når vi har udvalt sættet 20,28. så er det sæt der skal sammenlignes med ikke nødvendigvis 
    35,36,37,39,42.
    
    Det kan også være 35,36. for det er også et disjoint sæt.
    
    Så... Min test var for naiv.


cond1(test)
cond2(test)
mtcars$wt

lad os yderligere antage, at de ikke må være ens. så det er 7 forskellige tal der skal være der.

library(dplyr)
kand <- expand.grid(17:21, 28:32, 35:39, 36:40, 37:41, 39:43, 42:46)

mutate(kand, forsk=unique(Var1, Var2))

library(dplyr)
library(sets)

  kand %>%


%>% 
  mutate(forsk = unique(Var1, Var2, Var3, Var4, Var5, Var6, Var7, var48))

  
  svar: 20313839404245
  det er korrekt. Men det er satme langsomt.
  
En måde at få det til at køre hurtigere er ved at konstatere, at når jeg har dannet et komplet sæt af tallene - så har 
jeg også dannet alle de korresponderende, disjointe, sæt.

Udfordringen bliver dermed at få håndteret de sæt. Jeg har ikke helt gennemskuet hvordan jeg adresserer et enkelt 
sæt.

Det ligger og gemmer sig i funktionerne der tester om betingelserne er opfyldt.


  
============================
library(sets)  

test <- c(20,31,38,39,40,42,45)

cond1 <- function(test){
    result <- T
    set_test <- set_power(test)
    for(i in set_test){
      if(set_is_proper_subset(i,test) & !set_is_empty(i) & result){
        set_test2 <- set_complement(i,test)
        set_test2 <- set_power(set_test2)
        for(j in set_test2){
          if(set_is_proper_subset(j,test) & !set_is_empty(j)){
            if(sum(i)==sum(j)){
              result <- F
              break()
            }
          }
        }
      }
    }
    return(result)
  }

library(microbenchmark)
microbenchmark(cond1(test))
