The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any 
order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. 
The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.

Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

# Hvor mange gange kan man tage 5 elementer fra et sæt af 100 primtal?
  


#sudo su - -c "R -e \"install.packages('scales')\""
library(numbers)
primtal <- Primes(3,20000)
primtal


library(stringr)
concatprim <- function(x,y){
  # isPrime(as.integer(str_c(x,y,sep="")))
  isPrime(as.integer(str_c(x,y,sep="")))
}

primconcat <- function(x,y){
    # isPrime(as.integer(str_c(x,y,sep="")))
    isPrime(as.integer(str_c(y,x,sep="")))
  }

primconcat(5, primtal)

as.integer(str_c(primtal,5))

tet <- 1:5
primtal[concatprim(3,primtal)]
primtal[primconcat(3,primtal)]

tst <- primtal %>% 
  enframe() %>% 
  rowwise() %>% 
  mutate(kand1 = list(primtal[concatprim(value, primtal)])) %>% 
  rowwise() %>% 
  mutate(kand2 = list(primtal[primconcat(value,primtal)])) %>% 
  mutate(kand = list(intersect(unlist(kand1),unlist(kand2)))) %>% 
  select(value,kand) %>% 
  filter(length(kand)>0)

kandidater <- data.frame(i=numeric(), j=numeric(), k=numeric(), l=numeric(), m=numeric())

View(kandidater)

for(i in tst$value){
  for(j in unlist(tst[which(tst$value==i),]$kand)){
    for(k in intersect(unlist(tst[which(tst$value==i),]$kand) , unlist(tst[which(tst$value==j),]$kand)  )){
      for(l in intersect(intersect(unlist(tst[which(tst$value==i),]$kand) , unlist(tst[which(tst$value==j),]$kand)), unlist(tst[which(tst$value==k),]$kand))){
        for(m in intersect(
          intersect(
            intersect(unlist(tst[which(tst$value==i),]$kand) , unlist(tst[which(tst$value==j),]$kand)), 
            unlist(tst[which(tst$value==k),]$kand)),
          unlist(tst[which(tst$value==l),]$kand))){
          kandidater <- rbind(kandidater, sort(c(i,j,k,l,m)))
        }
      }
    }
  }
}
View(kandidater)
unlist(tst[which(tst$value==3),]$kand)

sort(c(5,1,2,3,4))

View(tst)
colnames(kandidater) <- c("i", "j", "k", "l", "m")
kandidater %>% 
  distinct() %>% 
  transmute(summen = i+j+k+l+m) %>% 
  min()
  
  
test <- primtal[concatprim(3,primtal)]

3, 7, 109, and 673
tester <- c(7,109,673)
tester %in% test

Vi skal finde fire tal, 3,7,109,673, "tal"
Der alle har det tilfælles, at 

combn(primtal,4)
