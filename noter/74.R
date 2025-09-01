# 74

library(dplyr)
library(microbenchmark)

fakts <- factorial(1:10)

%||%

Tag en vector, tilføj det næste led i kæden.

test <- as.list(169)
test <- append(test,170)

length(test)

tilf <- function(x){
  append(x,sum(factorial(as.numeric(unlist(str_split(x[[length(x)]],""))))))
}





tilf(test)

library(dplyr)
library(tibble)
listen <- 1:1000000 %>% 
  enframe(name=NULL) %>% 
  mutate(kæden = as.list(value)) %>% 
  mutate(kæden = map(kæden, tilf)) %>% 
  mutate(done=F)

tjek <- function(x){
  res <- F
  if(length(unlist(x))!=length(unique(unlist(x)))){
    res <- T
  }
  return(res)
}

listen[!listen$done,]$done <- unlist(map(listen[!listen$done,]$kæden, tjek))
oprliste <- listen
listen <- listen[!listen$done,]
listen[!listen$done,]$kæden <- (map(listen[!listen$done,]$kæden, tilf))
listen[!listen$done,]$done <- unlist(map(listen[!listen$done,]$kæden, tjek))
listen[!listen$done,]

999819   181
996213  3606
985334 10879
972513 12821
962467 10046
953457  9010
944195  9262
927387 16808
900550 26837
887160 13390
875549 11611
864243 11306
846436 17807
828636 17800
809870 18766
792597 17273
769698 22899
753101 16597
744831  8270
738166  6665
728519  9647
716673 11846
695713 20960
672475 23238
647610 24865
634995 12615
623829 11166
611783 12046
593184 18599
573759 19425
554459 19300
530417 24042
500326 30091
473417 26909
453634 19783
436699 16935
415571
388036
362157
320540
267463
221959
# først skriver vi en funktion, der returnerer summen af fakulteterne af cifrene i et tal.

listen




fac_dig_sum <- function(x){
  x %>% as.character() %>% 
    str_split("") %>% 
    map(as.numeric) %>% 
    map(factorial) %>% 
    map(sum) %>% 
    unlist
  }


# it turns out that there are only three such loops that exist
# så der er ingen grund til at spekulere så meget over loops. Enten ender kæden i et af de tre loops. Eller også 
# mapper det tilbage til sig selv.


tjek <- function(x){
  x <- unlist(x)
  res <- F
  if(length(x)!=length(unique(x))){res <- T}
  return(res)
}

naeste <- function(x){
  fac_dig_sum(x[[1]][[lengths(x)]])
}


# Så genererer vi en vektor med tallene fra 1 til 1.000.000
library(tibble)
listen <- 1:1000 %>% 
  enframe(name=NULL) %>% 
  mutate(kaeden=as.list(value)) %>% 
  mutate(done = F)

listen %>% 
  mutate(kaeden = map(kaeden, append(naeste()) ) )

tjek <- function(x){
  x <- unlist(x)
  res <- F
  if(length(x)!=length(unique(x))){res <- T}
  return(res)
}

naeste <- function(x){
  fac_dig_sum(x[[1]][[lengths(x)]])
}


# Dernæst initialiserer vi en vektor til at indeholde de tal, der resulterer i en kæde med 60 unikke tal.
resultat <- numeric()

# Så længe der er tal tilbage i listen:
while(length(listen)>0){
  print(length(listen))
  res <- numeric()           # initialiserer en tom vektor
  x <- sample(listen,1)      # udvælger et tilfældigt tal fra listen.
  print(x)
  res <- c(x, res)           # lægger det tilfældige tal i vektoren
  done <- F                  # Er vi færdige med at tjekke et tal?  
  while(!done){              # Så længe vi ikke er færdige med at tjekke et tal gør:
    næste <- fac_dig_sum(res[1])  # beregner det næste tal i kæden
    if(næste %in% res){           # Hvis tallet allerede er i kæden, har vi fundet den komplette kæde. Gør dernæst:
      done <- T                   # sæt done til sand, så vi ikke fortsætter
      if(length(res)==60){
        resultat <- unique(c(x, resultat))   # tilføj tallet til listen over tal der giver kæder på 60
      } else{                         # Kæden var ikke 60 lang. Et hvilket som helst tal i kæden vil ende i samme kæde. der er for kort.
                                      # Derfor fjerner vi alle tal i kæden fra listen.
        listen <- setdiff(listen, res)
        
      }
    }else{                           #tallet var ikke i listen allerede. Tilføj det.
      res <- c(næste,res)
    }
  }
}

# LÆS OPGAVEN! 
# it turns out that there are only three such loops that exist
# så der er ingen grund til at spekulere så meget over loops. Enten ender kæden i et af de tre loops. Eller også 
# mapper det tilbage til sig selv.

# det går i øvrigt aaaalt for langsomt.

x <- listen[i]
res <- c(res,x)

næste <- fac_dig_sum(res[1])
if(næste %in% res & ){
  result <- length(res)
} else{
  res <- c(næste,res)
}
length(setdiff(listen, res))

