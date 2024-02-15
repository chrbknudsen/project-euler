# 82

library(igraph)


# Dette er grundlæggende en øvelse i at optimere en rute i et netværk. Der er, i eksemplet 25 noder.
# 
# Giver vi dem fortløbende numre fra 1 til 25, kan vi i eksemplet se, at vejen 
# fra node 1 til node 2 er 673 enheder lang. Står vi i node 1, og går til node
# 2, øges summen med 673. Går vi den anden vej, fra 2 til 1, øges summen
# med 131.
# Fundamentalt samme øvelse som 83. Adjacencymatrixen skal blot laves om. Og så er det ikke fra
# celle 1 til celle sidst. Men den mindste af 80 forskellige mulige.




#Herunder er det ok.
# jeg genererer en celles nummer, nodens nummer, ud fra koordinaterne i matricen.

# Der er fem kolonner, og fem rækker:

nc <- 5
nr <- 5

# vi nummererer noderne fortløbende, fra venstre til højre, oppefra og ned, 1 til nc*nr. Lad os få den 
# nummerering ind i en matrix:
# 
# Først laver vi matricen
nrmat <- matrix(0, nc=nc, nr=nr)


# En celles nummer er givet ved:
#   rækkens nummer minus 1, ganget med antallet af kolonner. plus kolonnenummeret.

for(i in 1:nc){
  for(j in 1:nr){
    nrmat[i,j] <- (((i-1)*nc)+j)
  }
}

nrmat

# eller:
colnr <- function(x,y){
  (x-1)*nc+y
}

for(i in 1:nc){
  for(j in 1:nr){
    nrmat[i,j] <- colnr(i,j)
  }
}

nrmat

# Vupti. 
# Så genererer jeg en adjacency matrix, der viser hvilke veje der kan tages fra en given node til en anden 
# given node.

# Vi skal have lavet en adjacency matrix, der viser hvilke nodes der er forbundet med hvilke andre
# 
# Ser vi på matrixen med nummereringen af cellerne:
#   nrmat
# 
# kan vi se, at celle 17 er forbundet med cellerne 12, 16, 18 og 22. 
# 
# Vi skal være vågne. Det er fristende at konstatere, at de celler 17 er forbundet til er 17-5, 17-1, 17+1 og 
# 17+5. Det går alt når vi når til celle 10, som ikke er forbundet til celle 11.
# 
# Når matricen er 5*5, er der 5*5=25 rækker (og lige så mange konlonner) i adjacency matrixen.
# Sådan en laver vi.

adjm <- matrix(0, nc=nc*nr, nr=nc*nr)
adjm
# Og så løber vi igennem. Hvis to celler er forbundet, sættes værdien til 1.
# Vi kan bevæge os op, ned og til højre.
# i er rækkerne, j er kolonnerne. 

for(i in 1:nr){
  for(j in 1:nc){
    # if(j-1!=0){
    #   adjm[colnr(i,j), colnr(i,j-1)] <- 1  # et skridt til venste
    # }
    if(j+1<=nr){
      adjm[colnr(i,j), colnr(i,j+1)] <- 1 # et skridt til højre.
    }
    if(i-1 != 0){
      adjm[colnr(i,j), colnr(i-1,j)] <- 1  # et skridt op
    }
    if(i+1<= nc){
      adjm[colnr(i,j), colnr(i+1,j)] <- 1 # et skridt ned.
    }
  }
}
adjm
plot(graph.adjacency(adjm, weighted=T))

# Nice.

# Det er også denne der skal ændres når vi skal løse 81 og 82
# 
# Når vi går fra celle 1 til celle 2 - så er vejens længde, kantens længde, lig med værdien af celle 2.
# Går vi fra celle 2 til celle 1, så er vejens længde værdien af celle 1.
# 
# Så. for i,j i 1:25, er værdien lig j.
# Derfor skal jeg have en funktion, der går fra et node-tal, til en koordinat x,y i matricen med værdierne.

getxy <- function(z){
  x <- (z-1)%/%nc + 1
  y <- z -  (x-1)*nc
  return(c(x,y))
}



# Jeg skal som det allerførste have tallene ind i en matrix:
  
talmat <-   as.matrix(read.table(text=
" 131  673  234  103 18
      201  96  342 965 150  
      630  803  746  422  111
      537 699  497  121  956 
     805 732  524  37  331
    ", header=F))
  


colnames(talmat) <- 1:nc
# talmat
# 
# Nu har jeg en adjacency matrix, der beskriver hvor der er kanter i netværket.


# så skal jeg have lagt værdierne ind, så vi får fat på længden af kanterne.

# Når vi går fra celle 1 til celle 2 - så er vejens længde, kantens længde, lig med værdien af celle 2.
# Går vi fra celle 2 til celle 1, så er vejens længde værdien af celle 1.
# 
# Så. for i,j i 1:25, er værdien lig j.
# Derfor skal jeg have en funktion, der går fra et node-tal, til en koordinat x,y i matricen med værdierne.
# 
# 
# Så laver vi den endelige matrix. Hvor vi får værdierne ind.

nc
nr
finmat <- matrix(0, nc=nc*nr, nr=nc*nr)
for( i in 1:(nr*nc)){
  for(j in 1:(nr*nc)){
    finmat[i,j] <- talmat[getxy(i)[1],getxy(i)[2]]*adjm[i,j]
  }
}

finmat


g <- graph.adjacency(finmat, weighted = TRUE)
# kortestesti <- shortest_paths(g, 6,5)
# stien <- as.numeric(kortestesti$vpath[[1]])
# 
# MEN! Vi ved ikke hvilken celle vi skal starte i. Og hvilken vi skal slutte i.
# 
# Så vi skal gennem alle kombinationer af
# nrmat[,1]
# og
# nrmat[,nc]
# Dem har vi her:


stilength <- function(stien){
  resultatet <- 0
  for(i in 1:length(stien)){
  resultatet <- resultatet + as.numeric(talmat[getxy(stien[i])[1],getxy(stien[i])[2]])
  }
  return(resultatet)
}

# View(stier)
# shortest_paths(g, stier[2,1], stier[2,2])
# stier[2,]
for(i in 1:nrow(stier)){
  sti <- shortest_paths(g, stier[i,1],stier[i,2])
  sti <- sti$vpath[[1]]
  sti <- as.numeric(sti)
  sti <- stilength(sti)
  stier[i,3] <- sti
}

min(stier[,3])

Så skal grundlæggende lave det samme, blot for selve opgaven.

tallene <- readLines("p082_matrix.txt")
library(tibble)
talmat <- as_tibble(tallene) %>% 
  separate(value,into=as.character(1:80),sep=",",convert=TRUE) %>% 
  as.matrix()

nc <- 80
nr <- 80

# Så laver vi adjacencymatrixen
adjm <- matrix(0, nc=nc*nr, nr=nc*nr)

# Og så løber vi igennem. Hvis to celler er forbundet, sættes værdien til 1.

for(i in 1:nc){
  for(j in 1:nr){
    if(j-1!=0){
      adjm[colnr(i,j), colnr(i,j-1)] <- 1
    }
    if(j+1<=nr){
      adjm[colnr(i,j), colnr(i,j+1)] <- 1
    }
    if(i-1 != 0){
      adjm[colnr(i,j), colnr(i-1,j)] <- 1
    }
    if(i+1<= nc){
      adjm[colnr(i,j), colnr(i+1,j)] <- 1
    }
  }
}

# Og matrixen med numrene:
  nrmat <- matrix(0, nc=nc, nr=nr)

for(i in 1:nc){
  for(j in 1:nr){
    nrmat[i,j] <- (((i-1)*nc)+j)
  }
}

#  Så sætter vi tallene ind i matrixen, baseret på adjacency matrixen:
finmat <- matrix(0, nc=nc*nr, nr=nc*nr)
for( i in 1:(nr*nc)){
    for(j in 1:(nr*nc)){
      finmat[i,j] <- talmat[getxy(i)[1],getxy(i)[2]]*adjm[i,j]
    }
  }
  
# laver grafobjektet:  
g <- graph.adjacency(finmat, weighted = TRUE)

stier <- expand.grid(nrmat[,1], nrmat[,nc])
for(i in 1:nrow(stier)){
  sti <- shortest_paths(g, stier[i,1],stier[i,2])
  sti <- sti$vpath[[1]]
  sti <- as.numeric(sti)
  sti <- stilength(sti)
  stier[i,3] <- sti
}

min(stier[,3])
