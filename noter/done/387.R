387
Harshad numbers

Et tal der kan divideres med summen af dens cifre er et harshad tal. 

højretrunkerer vi et tal, og er det også et harshad tal, og kan vi fortsætte hele vejen - så er det et højre trunkerbart harshad tal
  Et harshad tal, der resulterer i et primtal, er et stærkt harshad tal. 
  
  
  
  Et primtal, der, højretrunkeret, giver et stærkt højretrunkertbart harshad tal, er et stærkt højre trunkertbart harshad primtal. 

  
  Så. stærke harshad tal der er højretrunkerbare. 
  
  Vi har nok ikke lyst til at finde alle primtal mindre end 10^14, og teste om de er stærke, højretrunkerbare harshad tal. 
  
  Vejen frem må være at finde alle højretrunkerbare harshad tal.  
  
  
  Hvilke tal fra 10 til 99 er harshad tal?
    
ciffersum <- function(x){
      sum(as.integer(unlist(str_split(x, ""))))
    }
ciffersum(10)  

library(dplyr)

harshad <- 1:9

i <- 1
del <- harshad[(floor(log10(harshad)))+1==i] %>% 
  map(tilf) %>% 
  unlist() %>% 
  enframe(name=NULL) %>% 
  filter(value %% sapply(value, ciffersum) == 0) %>% 
  .$value

harshad <- c(del, harshad)

harshad
i <- 2

del <- harshad[(floor(log10(harshad)))+1==i] %>% 
  map(tilf) %>% unlist() %>% 
  enframe(name=NULL) %>% 
  filter(value %% sapply(value, ciffersum) == 0) %>% 
  .$value

harshad <- c(del, harshad)
harshad


tilf <- function(x){
  x*10 + 0:9
}

i <- 1
for(i in 1:14){
  del <- harshad[(floor(log10(harshad)))+1==i] %>% 
  map(tilf) %>% 
  unlist() %>% 
  enframe(name=NULL) %>% 
  filter(value %% sapply(value, ciffersum) == 0) %>% 
    .$value

  
  harshad <- c(del, harshad)
}



Det var for mange:


harshad <- harshad[harshad < 10^14]

View(harshad)

Godt. Nu har jeg alle højretrunkerbare harshadtal. Nu skal vi så finde alle de stærke højretrunkerbare harshadtal.
De er karakteriseret ved at når tallet divideres med summen af sine cifre, får vi et primtal.

srthn <- harshad[harshad < 10^14] %>%                  # alle harshadtal under limit
  enframe(name=NULL) %>%                      # i dataframe
  mutate(cs = sapply(value, ciffersum)) %>%   # beregner ciffersummen
  mutate(prim = value/cs) %>%                 # beregner tallet divideret med ciffersummen. Det skal være et primtal.
  mutate(iseven = if_else(prim==2,F, prim%%2==0)) %>%   # starter med at beregne om det potentielle primtal er lige. Hvis det er to, er svaret falsk. Ellers resultatet af testen om det er lige.
  filter(!iseven)                  %>%            # filtrerer de potentielle primtal fra der er lige - og derfor ihvertfald ikke primtal
  mutate(primtal = map(prim, isPrime))  %>%       # så tilføjer vi en kolonne, der indeholder svaret på om det potentielle primtal er et primtal
  mutate(primtal = unlist(primtal))  %>%           # piller svaret ud af listerne.
  filter(primtal)

Problemet opstår her. iseven skal være falsk hvis det er 2. 

View(srthn)

Nu har vi så de højretrunkerbare harsahdtal, som, divideret med deres ciffersum, giver et primtal. Altså de stærke højretrunkerbare
harshadtal.


Nu mangler vi blot at finde alle de primtal, der, når de bliver højretrunkeret, giver et stærkt, højretrunkeret harshadtal.

Lad os lige teste under 10.000.
srthn <- srthn$value

l <- 10^14
options(scipen=999)
svaret <- srthn %>% 
  enframe(name=NULL) %>% 
  filter(value<l) %>%     # vi starter med at se på harshad tal under 10.000
  select(value) %>%                # og nøjes med value
  .$value %>%                      # dem piller vi ud
  map(tilf)  %>%                   # og un-højretrunkerer dem.
  unlist() %>%                          # og piller dem ud af listen. Dette er de potentielle primtal, som, når de højretrunkeres, ender med at være srthn
  enframe(name=NULL) %>%                # dem smider vi i en tibble
  filter(value < l) %>%              # vi var kun interesserede i dem under grænsen
  filter(value %% 2 != 0 )  %>%       # vi starter med at pille dem fra, der er lige. Her kan vi være ligeglade med 2!
  filter(value %% 3 != 0)  %>%            # det mindste tal er 211. Så vi ved at alle der er delelige med 3 ikke er primtal.
  filter(value %% 5 != 0) %>%               # ditto med 5
  filter(value %% 7 != 0 ) %>%    # og 7
  filter(value %% 11 != 0)  %>%           # og 11
  filter(value %% 13 != 0) %>% 
  filter(value %% 17 != 0) %>%               # og 13 og 17. alt dette for at mindske hvor mange tal der skal testes for primalitet.
  mutate(erprim = map(value, isPrime)) %>% 
  mutate(erprim = unlist(erprim)) %>% 
  filter(erprim) %>% 
  summarise(svar = sum(value))

svaret$svar

696067597313468 - 696067597313468

Med rettelsen af at 2 jo ikke skal sorteres fra tidligere, fordi det faktisk er et primtal. Så er vi i mål!

det er stadig for småt! 
  90619-90438
181. Enten er 181 et tal. eller også mangler der en del af de små.
divisors(20437)

%>% 
  rowwise() %>% 
  filter(isPrime(prim)) %>% 
  .$value

godt. det er de stærke, harshadtal.

Når nu vi tager dem, og tilføjer et højreciffer. Hvilke bliver så til primtal?

l <- 10000
options(scipen=999)  
harshad[harshad < l] %>% # tager alle harshadtal under grænsen
  enframe(name=NULL) %>%                                # smider dem i en df
  mutate(cs = unlist(map(value, ciffersum))) %>%        # beregner ciffersummen
  mutate(prim =  value/cs) %>%                          # beregner værdien divideret med ciffersummen   
  mutate(iseven = if_else(prim%%2==0,T, prim==2)) %>%   # beregner om tallene er lige
  filter(!iseven) %>%                                   # filtrerer alle de lige resultater fra.
  filter(unlist(map(prim, isPrime)))  %>% 
  tail() # filtrerer de fra der ikke er primtal
  .$value %>% 
  map(tilf) %>% 
  unlist



