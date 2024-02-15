85

sådan et grid er givet ved x og y.



Hvis der er 3 tern horisontalt, kan x antage værdierne 1 til 4.

Lad os kalde grænsen
h <- 4

x er da 
x <- 1:h

Der er 2 tern vertikalt.
Så kan y tage værdierne 1 til 3
  
v <- 3
y <- 1:v


Sådan et rektangel er defineret af nedre venstre hjørne, og øvre højre hjørne.

Mulige nedre venstre hjørner er defineret ved 
x1 fra 1:(h-1)
y1 fra 1:(v-1)

Mulige øvre højre hjørner er så defineret ved at x2 er mindst en større end x1, men mindre end h
x2 fra (x1+1):h
y fra (y1+1):v

eller

så for given x1, kan x2 tage h-x1 forskellige værdier.
For given y1 kan y2 tage v-x1 forskellige værdier.

x1 = 1
y1 = 1

x har så 
h - 1
muligheder
y
v -1
eller ialt

(h-x1)*(v-y1)

x1 <- 1:(h-1)
y1 <- 1:(v-1)  

h <- 1
v <- 100
giver 24½ million
library(dplyr)
find_antal <- function(h,v){
expand.grid(1:(h-1),1:(v-1)) %>% 
  mutate(antal=(h-Var1)*(v-Var2)) %>% 
  summarise(antal=sum(antal))
}

h <- 10
v <- 12
1:(h-1)*(1:(v-1))

find_antal(3,4)


1x1000 giver 

499 500

library(dplyr)


kand <- expand.grid(2:100, 2:100) %>% 
  mutate(antal=map2(Var1, Var2, find_antal)) %>% 
  mutate(antal = unlist(antal))

View(kand)

kand %>% 
  mutate(dist=2000000 - antal) %>% 
  arrange(abs(dist)) %>% 
  slice(1) 

77*36

%>% 
  summarise(svar=(Var1-1)*(Var2-1))

det er så ved 78x37

eller 77x36 tern. 
Er jeg sikker på at det er et globalt minimum?
  Næh. Men 77 og 36 er pænt langt fra de grænser jeg undersøger. Og meget tæt på 2.000.000

Så lad os bare prøve.

h <- 20
x <- 1
length((x+1):h)
