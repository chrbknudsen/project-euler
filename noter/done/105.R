105

Grundlæggende samme øvelse som 103. Test hver enkelt sæt i 105_sets.txt, og se om de opfylder kriterierne.

Nu har vi fået en hurtigere måde at teste.

data <- readLines("p105_sets.txt")
library(stringr)
test <- as.numeric(unlist(str_split(data[2],",")))

library(sets)

getsets <- function(x){
  z <- set_power(x)
  y <- list()
  j <- 1
  for(i in z){
    if(set_is_proper_subset(i,x) & !set_is_empty(i)){
      y[[j]] <- i
      j <- j+1
    }
  }
  return(y)
}

bet1 <- function(x){
  result <- T
  test <- getsets(x)
  summer <- unlist(lapply(test,sum))
  test2 <- test[duplicated(summer) | duplicated(summer, fromLast = TRUE)]
  for(i in test2){
    for(j in test2){
      if(sum(i)==sum(j) & set_is_empty(set_intersection(i,j))){
        result <- F
        return(result)
      }
    }
  }
  return(result)
}

sum_x_mindste <- function(y,x){
  y <- sort(y)
  sum(y[1:x])
}

sum_x_største <- function(y,x){
  y <- sort(y)
  y <- rev(y)
  sum(y[1:x])
}

sum_x_mindste(test,1)



bet2 <- function(test){
  res <- T 
  for(i in 2:length(test)){
    if(sum_x_mindste(test,i)<sum_x_største(test,i-1)){
      res <- F
    }
  }
  return(res)
}

cond12 <- function(t){
  res <- 0
  if(bet2(t)){
    if(bet1(t)){
    res <- sum(t)
    }
  }
  return(res)
}

svar <- 0
for(i in 1:length(data)){
  
  svar <- svar + cond12(as.numeric(unlist(str_split(data[i],","))))
  
}
er svaret. Det skal jeg gerne få...
svar


Jeg får:
87125

Noget er galt. Der er sæt der bliver godkendt uden at være korrekt...