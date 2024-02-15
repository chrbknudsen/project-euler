# 72
# 
# Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.
# 
# If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:
#   
#   1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
# 
# It can be seen that there are 21 elements in this set.
# 
# How many elements would be contained in the set of reduced proper fractions for d ≤ 1,000,000?
#   
#   det er noget med primfaktorer.

en oneliner:

sum(unlist(map(1:1000000, eulersPhi)))-1

Ikke super hurtigt. Men det giver det rigtige svar.

  

library(numbers)
library(dplyr)
library(purrr)

L <- 1000000

df <- data.frame(n=1:L, p=NA)

primtal <- Primes(L)


# Det gælder at phi(n) = n - 1 hvor n er et primtal

df[primtal,2] <- primtal-1

# vi ved at 
# phi(2m) = 2*phi(m) hvor m er lige
# og 
# phi(2m) = phi(m) hvor m er ulige
# 
# Så alle tal m = 2*n hvor n er et primtal, har phi(m) = phi(n)
# 
# vi skal kun bruge primtal mindre end L/2

midl_prim <- primtal[primtal<L/2]


df[df[midl_prim[-1],1]*2,2] <- df[df[midl_prim[-1],1],2]


der er så
sum(is.na(df$p))

879965 tilbage

alle lige n

df[which(df$n%%2==0),]

alle lige n hvor vi har p
df[which(df$n%%2==0 & !is.na(df$p)),1]

alle n vi så kan beregne som 2n

midl <- 2*df[which(df$n%%2==0 & !is.na(df$p)),1]

Vi er kun interesserede i de der er mindre end L/2

midl <- midl[midl<L/2]

phi(2*n) er så lig med 2*phi(n)

så
df[midl,2] <- df[midl/2,1]


der er så
sum(is.na(df$p))

868231
tilbage

k <- 3
df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))

sum(is.na(df$p))
868215
Det kan vi gentage for alle ulige k



k <- 9
df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))

sum(is.na(df$p))

k <- 15
df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))

sum(is.na(df$p))
868181

k <- 21
df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))

sum(is.na(df$p))

k <- 25
df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))

sum(is.na(df$p))

k <- 27
df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))

sum(is.na(df$p))

k <- 33
df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))

sum(is.na(df$p))

868118

Lad os lige tage alle de lige igen.


midl <- 2*df[which(df$n%%2==0 & !is.na(df$p)),1]
midl <- midl[midl<L/2]
df[midl,2] <- df[midl/2,1]
sum(is.na(df$p))

861844

det var trods alt en del flere.

k <- 35
df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))

sum(is.na(df$p))
861829

har vi en liste over alle de ulige tal?
  
df[which(df$n%%2==1 & is.na(df$p)),1]

lad os lige få elimineret 1. 
df[1,2] <- 1

Vi napper lige 10 stykker af de ulige

for(k in df[which(df$n%%2==1 & is.na(df$p)),1][1:10]){
  df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))
}

sum(is.na(df$p))
861682
og napper de lige derefter

midl <- 2*df[which(df$n%%2==0 & !is.na(df$p)),1]
midl <- midl[midl<L/2]
df[midl,2] <- df[midl/2,1]
sum(is.na(df$p))

858315

lad os tage 100 til
for(k in df[which(df$n%%2==1 & is.na(df$p)),1][1:100]){
  df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))
}

Og dernæst de lige
midl <- 2*df[which(df$n%%2==0 & !is.na(df$p)),1]
midl <- midl[midl<L/2]
df[midl,2] <- df[midl/2,1]

sum(is.na(df$p))
855229

Vi gentager. denne gang med 1000

for(k in df[which(df$n%%2==1 & is.na(df$p)),1][1:1000]){
  df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))
}

midl <- 2*df[which(df$n%%2==0 & !is.na(df$p)),1]
midl <- midl[midl<L/2]
df[midl,2] <- df[midl/2,1]

sum(is.na(df$p))

844439

og igen

for(k in df[which(df$n%%2==1 & is.na(df$p)),1][1:1000]){
  df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))
}

midl <- 2*df[which(df$n%%2==0 & !is.na(df$p)),1]
midl <- midl[midl<L/2]
df[midl,2] <- df[midl/2,1]

sum(is.na(df$p))

835604

og igen

for(k in df[which(df$n%%2==1 & is.na(df$p)),1][1:1000]){
  df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))
}

midl <- 2*df[which(df$n%%2==0 & !is.na(df$p)),1]
midl <- midl[midl<L/2]
df[midl,2] <- df[midl/2,1]

sum(is.na(df$p))

View(df)

Problemet er her, at jeg kun får elimineret lige tal. Og et enkelt ulige i hvert loop.
Hvilket jo grundlæggende betyder at jeg skal beregne phi for alle ulige tal.

der er
length(df[which(df$n%%2==1 & is.na(df$p)),1])
411385 
ulige tal tilbage nu.
length(df[which(df$n%%2==0 & is.na(df$p)),1])

og 
371083
lige.

tager vi 1000 til

for(k in df[which(df$n%%2==1 & is.na(df$p)),1][1:1000]){
  df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))
}

midl <- 2*df[which(df$n%%2==0 & !is.na(df$p)),1]
midl <- midl[midl<L/2]
df[midl,2] <- df[midl/2,1]

fjernes der præcist 1000 ulige tal.
411385 - length(df[which(df$n%%2==1 & is.na(df$p)),1])
 
Men 5092 lige
371083-length(df[which(df$n%%2==0 & is.na(df$p)),1])

Så hver gang jeg kører loopet med de 1000, får jeg elimineret ca. 6000 tal. Omend det kommer til at falde efterhånden som vi 
kommer deropad.

det betyder at jeg skal køre det mindst 129 gange til.
sum(is.na(df$p))/6000

Så jeg har egentlig brug for at kunne generere de ulige hurtigere. det lader ikke til at være muligt.


Hvis p er et primtal, og k >1. Såer phi (p^k) = p^(k-1)*(p-1)
k <- 2
Primes(100) %>% 
  enframe(name=NULL) %>% 
  dplyr::rename("n" = "value") %>% 
  mutate(p = n^(k-1)*(n-1)) %>% 
  mutate(n=n^k)
  
Tager vi derfor 
primtal[primtal<sqrt(L)]

så vil 
primtal[primtal<sqrt(L)]^2

have phier der er
primtal[primtal<sqrt(L)]*(primtal[primtal<sqrt(L)]-1)

derfor
df[primtal[primtal<sqrt(L)]^2,2] <- primtal[primtal<sqrt(L)]*(primtal[primtal<sqrt(L)]-1)

Så kører vi den lige igen
for(k in df[which(df$n%%2==1 & is.na(df$p)),1][1:1000]){
  df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))
}

midl <- 2*df[which(df$n%%2==0 & !is.na(df$p)),1]
midl <- midl[midl<L/2]
df[midl,2] <- df[midl/2,1]

sum(is.na(df$p))
760012

585526
for(k in df[which(df$n%%2==1 & is.na(df$p)),1][1:1000]){
  df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))
}

midl <- 2*df[which(df$n%%2==0 & !is.na(df$p)),1]
midl <- midl[midl<L/2]
df[midl,2] <- df[midl/2,1]

sum(is.na(df$p))

df[df[which(df$n%%2==1 & is.na(df$p)),1][1:10000],2] <- unlist(map(df[which(df$n%%2==1 & is.na(df$p)),1][1:10000], eulersPhi)) 

sum(is.na(df$p))


phi(2m) = phi(m) hvor m er ulige

Så. det er alle de ulige, hvor p er kendt. 
df[df[which(df$n%%2==1 & !is.na(df$p) & df$n < L/2),1]*2,2] <- df[which(df$n%%2==1 & !is.na(df$p)),2]

df <- df[!is.na(df$n),]

==================
der er så
sum(is.na(df$p))

879965 tilbage

ganger vi dem med 2, 


Anallet af reduced proper fractions for en given d er eulersPhi(d).
Så det er "bare" at finde eulersPhi for alle d  ≤ 1,000,000 og lægge dem sammen.

Det tager bare en krig...

library(numbers)
library(dplyr)
library(purrr)

1:30 %>% 
  enframe(name=NULL) %>% 
  mutate(PHI=map(value, eulersPhi)) %>% 
  mutate(PHI=unlist(PHI)) %>% 
  View()

reglen om at phi(2m) = 2*phi(m) hvor m er lige
og phi(2m) = phi(m) hvor m er ulige

Kan skære antallet af beregninger voldsomt ned. 

eulersPhi(3)
så phi af 6 er lig phi af 3. eller 2, fordi phi(3)=1

phi af 12 er så lig med 2*2 = 4 fodi phi(6) = 2

Så når jeg har phi(3), har jeg phi(2*3)=phi(6) og ph(4*3)=phi(12)

når jeg har phi(12) har jeg også phi(24) og phi(48) og derudaf.

Så jeg kan med simpel aritmetik skaffe ret mange, bare ved at beregne phi(3).

Hvordan populerer jeg sådan en fætter?
  
  3 giver 6, giver 12, giver 24, giver 48, giver etc.

Så givet 3, vil jeg generere vektoren:

  tre <- c(3,6,12,24,48, 96, 192)
Den kan jeg genere ved:

  
   3*2^(0:6)
Lad os lige generere phier for dem.

(3*2^(0:10)) %>% 
  enframe(name=NULL) %>% 
  mutate(PHI=map(value, eulersPhi)) %>% 
  mutate(PHI=unlist(PHI))

det er de værdier for hvilke jeg let kan beregne phi når jeg har phi(3)
Hvad er de så?
  m er odd, så phi af 6 er lig phi af 3.
Alle de efterfølgende er lige, så hvert led er dobbelt så stort som det foregående.

Hvis vi så tager 

2^c(0,0,1:9)*eulersPhi(3)

Cool. Det virker.

Så hvis jeg laver en df med n 1:1000000, kan jeg populere den baseret på vektorer. Det er fint. 
Hvordan beregner jeg hvor langt jeg skal gå i hver?
  
  eller, hvor stor i skal jeg vælge her
3*2^(0:i)  

når den maks skal være 1000000

eller, for et givet ulige tal, k, og grænse L = 1000000

k*2(0:i), find maksimalt i.

log2(L/k)

Så
k <- 3
L <- 1000000

Disse værdier af n
k*2^(0:floor(log2(L/k)))
har phi(n)
eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))


df <- data.frame(n=1:1000000, phi = NA)

df[Primes(L),2] <- Primes(L) - 1

sum(is.na(df$phi)) (921502)
k <- 3
df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))

k <- 7
df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))

for(k in Primes(3,L)){
  df[k*2^(0:floor(log2(L/k))),2] <- (k-1)*2^c(0,0:floor(log2(L/k)-1))
  print(k)
}

sum(is.na(df$phi)) (832537)

Nu kunne vi gå i gang med at pille alle de ulige ud.
k <- 9
df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))

sum(is.na(df$phi)) (832520)

Men 9 henter kun 17 tal

k <- 15
df[k*2^(0:floor(log2(L/k))),2] <- eulersPhi(k)*2^c(0,0:floor(log2(L/k)-1))

sum(is.na(df$phi)) (832503)
det gør 15 så også, men det batter ikke rigtigt, når der mangler 832.503 n.


Noget mere drastiks. Dette er alle de ulige tal i dataframen, der har en phi-værdi.
test <- df[which(df$n%%2==1 & !is.na(df$phi)),1]

dem er der 78499 af.
length(test[test<L/2])

41539 af dem er mindre end halvdelen af grænsen.

dem kan vi få fat i her
df[which(df$n%%2==1 & !is.na(df$phi) & df$n < L/2),1]

det vi ved er, at efterson disse n er ulige, er phi(2*n) lig phi(n)
Så disse værdier af n
  df[which(df$n%%2==1 & !is.na(df$phi) & df$n < L/2),1]*2

  har disse værdier af phi
  df[which(df$n%%2==1 & !is.na(df$phi) & df$n < L/2),2]
  
  det skulle betyde, at 
df[df[which(df$n%%2==1 & !is.na(df$phi) & df$n < L/2),1]*2,2]  <- df[which(df$n%%2==1 & !is.na(df$phi) & df$n < L/2),2]
  
sum(is.na(df$phi)) (832503)

det lader til at have givet absolut ingenting. Og selvfølgelig har det ikke det. De eneste ulige n der har phi-værdier i df, er 
primtallene, 9 og 15. Og det dobbelte af alt det er allerede lagt i.  Så der skal findes på noget bedre.

Men vi kan gøre noget tilsvarede for de lige værdier af n.

df[which(df$n%%2==0 & !is.na(df$phi) & df$n < L/2),1]

For disse værdier af n har vi en værdi af phi.
da vi ved at phi(2m) = 2*phi(m) for m lige, ved vi, at disse værdier

df[which(df$n%%2==0 & !is.na(df$phi) & df$n < L/2),1]*2

har phi = df[which(df$n%%2==0 & !is.na(df$phi) & df$n < L/2),2]*2


df[df[which(df$n%%2==0 & !is.na(df$phi) & df$n < L/2),1]*2,2] <- df[which(df$n%%2==0 & !is.na(df$phi) & df$n < L/2),2]*2

View(df)

sum(is.na(df$phi)) (832503)

sum(is.na(df$phi)) (832503)
det gav heller ikke en skid...
Og jeg kan ikke gro dataframen på den måde. For de eneste lige værdier er beregnet ud fra andre værdier. Og for dem alle gælder,
at det efterfølgende dobbelt så store led er beregnet.

Så...
På den anden side...
Jeg har ikke fanget n=8 endnu. Og jeg har n=4.

Problemet er at den der hvor jeg dobler kun giver en ekstra pr. iteration. Det tager for lang tid.

Hvad med kvadraterne af primtallene?

View(df[df[which(df$n%%2==0 & !is.na(df$phi) & df$n < L/2),1]*2,])
df[which(df$n%%2==0 & !is.na(df$phi) & df$n < L/2),]
head(df)


derfor - 

Og vi ved at 
length(test)
View(df)
eulersPhi(2792)
View(df)


df
eulersPhi(786432)



eulersPhi(786432)

det er 2^0:i hvor det udtryk max må vre 1000000/3
2^i = 333333
Eller, mere generelt

grænsen for i går ved:
log2(L/k)

2^1

Så position 1 er phi(3)*1
Position 2 er phi(3)*1