118

library(numbers)
library(tidyverse)

der skal bruges primtal. Og mange af dem.
Vi starter med at generere alle relevante 8-cifrede primtal.
De er på max 8 cifre. Der indgår ikke et 0. Og hvert af cifrene 1 til 9 optræder højest en gang.

De 9-cifrede klarer vi på anden vis (på en anden maskine)

prim_kand <- Primes(99000000)
Godt så. De kan ikke være større end 99000000. For så er der mere end et 9-tal.

Nu skal vi så have skåret dem til.

Ingen 0er
prim_kand <- prim_kand %>% 
  enframe(name=NULL) %>% 
  filter(str_count(value, "0")==0)


Tallene 1 til 9 må kun optræde en gang.

prim_kand <- prim_kand %>% 
  filter(str_count(value, "1")<=1) %>% 
  filter(str_count(value, "2")<=1) %>% 
  filter(str_count(value, "3")<=1) %>% 
  filter(str_count(value, "4")<=1) %>% 
  filter(str_count(value, "5")<=1) %>% 
  filter(str_count(value, "6")<=1) %>% 
  filter(str_count(value, "7")<=1) %>% 
  filter(str_count(value, "8")<=1) %>% 
  filter(str_count(value, "9")<=1)

Så er vi nede på et mere overkommeligt antal.

en <- prim_kand %>% 
  filter(nchar(value)==1) %>% 
  .$value

to <- prim_kand %>% 
  filter(nchar(value)==2)%>% 
  .$value

tre <- prim_kand %>% 
  filter(nchar(value)==3)%>% 
  .$value

fire <- prim_kand %>% 
  filter(nchar(value)==4)%>% 
  .$value

fem <- prim_kand %>% 
  filter(nchar(value)==5)%>% 
  .$value

seks <- prim_kand %>% 
  filter(nchar(value)==6)%>% 
  .$value

syv <- prim_kand %>% 
  filter(nchar(value)==7)%>% 
  .$value

otte <- prim_kand %>% 
  filter(nchar(value)==8)%>% 
  .$value


9 kan partioneres på følgende måder:
[1, 1, 1, 1, 1, 1, 1, 1, 1]  
[2, 1, 1, 1, 1, 1, 1, 1]   
[2, 2, 1, 1, 1, 1, 1]    
[3, 1, 1, 1, 1, 1, 1]    
[2, 2, 2, 1, 1, 1]    
[3, 2, 1, 1, 1, 1]    
[4, 1, 1, 1, 1, 1]    
[2, 2, 2, 2, 1]    
[3, 2, 2, 1, 1]    
[3, 3, 1, 1, 1]    
[4, 2, 1, 1, 1]    
[5, 1, 1, 1, 1]    
[3, 2, 2, 2]    
[3, 3, 2, 1]    
[4, 2, 2, 1]    
[4, 3, 1, 1]    
[5, 2, 1, 1]    
[6, 1, 1, 1]    
[3, 3, 3]    
[4, 3, 2]    
[4, 4, 1]    
[5, 2, 2]    
[5, 3, 1]    
[6, 2, 1]    
[7, 1, 1]    
[5, 4]   
[6, 3]   
[7, 2]   
[8, 1]   
[9]
  
Nogen af dem kan udelukkes på forhånd - der er kun 4 encifrede primtal.
[2, 2, 2, 1, 1, 1]    
k222111 <- expand.grid(to,to,to, en, en, en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3, Var4,Var5,Var6))


[3, 2, 1, 1, 1, 1]    
k321111 <- expand.grid(tre,to,en,en,en,en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3, Var4,Var5,Var6))

[2, 2, 2, 2, 1]    
k22221 <- expand.grid(to,to,to,to,en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3, Var4,Var5)) 


[3, 2, 2, 1, 1]    
k32211 <- expand.grid(tre,to,to,en,en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3, Var4, Var5)) 


[3, 3, 1, 1, 1]    
k33111<- expand.grid(tre,tre, en, en, en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3, Var4, Var5)) 

[4, 2, 1, 1, 1]    
k42111 <- expand.grid(fire,to,en,en, en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3, Var4, Var5))

[5, 1, 1, 1, 1]    
k51111 <- expand.grid(fem,en, en, en, en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3, Var4, Var5))

[3, 2, 2, 2]    
k3222 <- expand.grid(tre,to,to,to)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3, Var4))

[3, 3, 2, 1]    
k3321 <- expand.grid(tre, tre, to, en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3, Var4))

[4, 2, 2, 1]    
k4221 <- expand.grid(fire,to,to,en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3, Var4))

[4, 3, 1, 1]    
k4311 <- expand.grid(fire,tre, en,en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3, Var4))

[5, 2, 1, 1]    
k5211 <- expand.grid(fem,to,en,en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3, Var4)) 

[6, 1, 1, 1]    
k6111 <- expand.grid(seks,en,en, en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3, Var4))

[3, 3, 3]    
k333 <- expand.grid(tre,tre,tre)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3)) 

[4, 3, 2]    
k432 <- expand.grid(fire,tre,to)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3)) 

[4, 4, 1]    
k441 <- expand.grid(fire,fire,en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3)) 

[5, 2, 2]    
k522 <- expand.grid(fem,to,to)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3)) 

[5, 3, 1]    
k531 <- expand.grid(fem,tre,en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3)) 

[6, 2, 1]    
k621 <- expand.grid(seks,to,en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3)) 


[7, 1, 1]    
k711 <- expand.grid(syv,en,en)  %>% 
  transmute(konkat=str_c(Var1,Var2, Var3))

[5, 4]   
k54 <- expand.grid(fem,fire) %>% 
  transmute(konkat=str_c(Var1,Var2)) 

[6, 3]   
k63 <- expand.grid(seks,tre) %>% 
  transmute(konkat=str_c(Var1,Var2)) 

[7, 2]   
k72 <- expand.grid(syv,to) %>% 
  transmute(konkat=str_c(Var1,Var2)) 
  
[8, 1]
k81 <- expand.grid(otte,en) %>% 
  transmute(konkat = str_c(Var1,Var2))





===================================

    
samlet <- rbind(k222111 ,
k321111 ,
k22221 ,
k32211 ,
k33111,
k42111 ,
k51111 ,
k3222 ,
k3321 ,
k4221 ,
k4311 ,
k5211 ,
k6111 ,
k333 ,
k432 ,
k441 ,
k522 ,
k531 ,
k621 ,
k711 ,
k54 ,
k63 ,
k72 ,
k81 )

View(22221)

k4311 <- k4311 %>% 
  filter(str_count(konkat, "1")==1)

samlet <- samlet %>% 
  filter(str_count(konkat, "9")==1)


nrow(samlet)

Den sidste kan også udelukkes. Der er slet ikke nogen.


  