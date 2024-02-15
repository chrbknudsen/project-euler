125





n <- 200
sum((1:n)^2)

øvre grænse
sqrt(10^8)

10000

men

n <- 7071

så. Vi har squares fra 1 til 7071

Vi skal have taget alle kombinationer af dem.

f <- function(x,y){
  sum((x:y)^2)
}

expand.grid(1:7071, 1:7071) %>% 
  filter(Var1<Var2) %>% 
  mutate(sum = map2(Var1, Var2, f)) %>% 
  filter(sum <10^8)

Hvordan sikrer jeg mig at jeg ikke kan finde en der er større?
  

k <- 10000000

primeFactors(k^2 + 16)
 
Måske snarere - test om et tal kan skrives som summen af et antal konsekutive kvadrater.

595
Det største kvadrat der kan komme på tale er:

  
t <- 595
done <- F
while(!done){
  for(i in floor(sqrt(t)):2){
    j <- i
    rest <- t
    while(rest>0){
      rest <- rest - j^2
      j <- j-1
    }
    if(rest==0){
      done=T
      print(i)
    }
  }
  
}

testden <- function(t){
  done <- F
  while(!done){
    for(i in floor(sqrt(t)):2){
      j <- i
      rest <- t
      while(rest>0){
        rest <- rest - j^2
        j <- j-1
      }
      if(rest==0){
        done=T
        
      }
    }
    
  } 
  return(done)
}

testden(595)

testden(3)

sqrt(3)

for(k in 11:1000){
  if(testden(k)){
    print(k)
  }
}
