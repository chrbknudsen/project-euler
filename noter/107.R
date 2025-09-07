# 107
tabel <- readLines("p107_network.txt")

library(dplyr)
library(igraph)
library(tibble)
library(stringr)
library(tidyr)
library(ggraph)
tabel

tabel %>% 
  enframe()  %>% 
  separate(value, paste("V", 1:40, sep=""), sep=",") %>% 
  View()

mat <- tabel %>% 
  enframe()  %>% 
  separate(value, paste("V", 1:40, sep=""), sep=",") %>% 
  apply(c(1,2), function(x) ifelse(x=="-", 0, as.numeric(x)) ) %>% 
  as.data.frame() %>% 
  select(-name) %>% 
  as.matrix() 
  
colnames(mat) <- NULL
rownames(mat) <- paste("V", 1:40, sep="")
View(mat)

g1 <- graph_from_adjacency_matrix(mat, weighted=T, mode="undirected") 

sum(strength(mst(g1)))

E(mst(g1))
adjm <- as_adjacency_matrix(mst(g1))
sum(adjm*mat)/2


sum(mat)/2 - sum(adjm*mat)/2

Fordi - det er ikke vægten af mst der er svaret. Det er vægten af det sparede, altså vægten af det oprindelige netværk minus vægten af det minimale netværk der er svaret...

Så lær dog at læse hele opgaven!

library(magic)
library(Matrix)
View(mat)
graph_f
graph.strength(mst(g1), mode="all")

adja

  plot(mst(graph_from_adjacency_matrix(mat, weighted=T, mode="undirected") ))

  demo(package="igraph")

 

kortest <- mst(test, algorithm = "prim")
igraph::as_data_frame(kortest)
as_
as_adjacency_matrix(kortest)

strength(kortest, mode="out")

?strength
  

V(kortest)

set.seed(42)
nzs <- function(x) sort(x [x!=0])
adjm <- matrix(runif(100), 10)
adjm[ adjm<0.5 ] <- 0
adjm

g3 <- graph_from_adjacency_matrix((adjm + t(adjm))/2, weighted=TRUE,
                                  mode="undirected")
plot(g3)
