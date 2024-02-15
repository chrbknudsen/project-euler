173

Det totale kvadrat har et areal på den ydre dimension i anden.
Det indre kvadrat har et areal på den indre dimension i anden.

Arealet af den søgte struktur er 
det første areal - det andet areal.

Vi ved at med 32 kvadrater, kan vi lave to af de her dimser.

Det kan vi, fordi

y^2 - i^2 =32
har to løsninger i y,i

y=6, i = 2
og
y=9, i = 7

Det er givet at i er i intervallet 1 til y-2

og at y er i intervallet 3 til sqrt(l)-1, hvor l er det antal tiles vi har at gøre godt med.

Har vi 100 tiles, kan y være i intervallet 3:10
og i i 1:sqrt(l)-2. her 1:8

Nope. Grænsen for den ydre er langt større. 


y^2 - (y-2)^2 = 100

y^2 - (y^2 + 4 - 4y) = 100
y^2 - y^2 - 4 + 4y = 100
- 4 + 4y = 100
4y = 100 + 4
y = l/4 + 1

dvs at når l = 100, er den ydre grænse givet ved 3:26
Og den indre er stadig givet ved 1:y-2, dvs 1:24


Hvor mange af kombinationerne 1:8 3:10 holder sig under 100?

  der ligger yderligere en symmetribegræsning.
Hvis den ydre er 5, kan den indre ikke være 4.

Når så vi når til l = 1000000
l <- 100
library(dplyr)


f <- function(x){
  bund <- if_else(x^2<l, 1, floor(sqrt(x^2-l)))
  bund <- if_else(bund%%2==0, bund, bund-1)
  if(x%%2==0){c(seq(bund,(x-2), by=2))}else{c(seq(bund,(x-2), by=2))}
}

x<-6
f(6)

Den nedre grænse for den indre sidelængde er 1.
Men hvis ydre sidelængde i anden, er større ned l (det antal tiles der max kan bruges), så kan indre sidelængde ikke være mindre
end

sqrt(ydre længde i anden - grænsen)

Øvre grænse er under alle omstændigheder ydre længde minus 2

Så - den ydre sidelængde kan være mellem 3 og l/4+1
indre mellem 1 og l/4-1

for given ydre sidelængde 
y <- 200000

sqrt(y^2 - l)

i <- sqrt((l/4+1)^2-y^2)
y^2 - i^2

x <- 1000000/4

l <- 1000000
library(tidyverse)
library(purrr)
3:(l/4+1) %>% 
  enframe(name=NULL) %>% 
  mutate(komb = map(value, f)) %>% 
  unnest() 
%>%
  filter(value%%2 == komb%%2) %>% 
  mutate(antal_tiles = value^2 - komb^2) %>% 
  filter(antal_tiles <= 100) 
  

det bliver for stort... Hm. Vi kan ca. halvere ved nedenstående.

y^2 - i^2 = l
y

Når vi ved at y^2 er noget bestemt. Så er der en nedre grænse for is størrelse.
i^2 = l - y^2 

A = 100
y mellem 3 og l/4 + 1.

her l = 100
når y er max, dvs 26
Det vil sige, at 
676-100
i skal være mindst
sqrt(576)


Så er arealet - uden indre 26^2




Det giver i princippet dem alle.  Men der er alt for mange. Hvis den ydre sidelængde er lige, skal den indre også være det.
Er den ydre sidelængde ulige, skal den indre også være det
Så. Hvordan nøjes jeg med at generere det?
Det er denne der skal modificeres. 

Den skal laves med en seq i stedet.

for lige tal:
  (4%%2)
seq(l%%2 +2, l, by=2)

for ulige tal

l%%2, l, by=2



  f <- function(l){
    1:(l-2)
  }
  
  
expand.grid(3:(l/4 + 1), 1:(l/4-1)) %>% 
  mutate(antal_tiles = Var1^2-Var2^2) %>% 
  filter(Var1>Var2) %>% 
  mutate(diff = Var1-Var2) %>% 
  filter(diff>1) %>% 
  filter(antal_tiles <= l) %>% 
  filter(diff%%2==0) %>% 
  nrow()

Det giver alt for meget. Så hvordan genererer jeg de kombinationer uden at lave dem alle.



===============================================
  l <- 1000000

Nedre grænse for y: 3
Øvre grænse for y: l/4+1 (så kan den ikke blive større)

y_max = l/4+3



Øvre grænse for den indre: y-2
Nedre grænse for indre 1


Når y^2 > l kan vi sætte en ny nedre grænse for i.

f <- function(x,y){
  seq(x,y,by=2)
}

library(tidyverse)
library(purrr)
3:y_max %>% 
  enframe(name=NULL) %>% 
  rename(y=value) %>% 
  mutate(i_max = y-2) %>% 
  mutate(i_min = if_else(y^2>l, floor(sqrt(y^2-l)), 2)) %>% 
  mutate(i_min = if_else(y%%2==0 & i_min%%2!=0 & i_min>1, i_min-1, i_min)) %>% 
  mutate(i_min = if_else(y%%2!=0 & i_min%%2==0, i_min-1, i_min)) %>% 
  mutate(i = map2(i_min, i_max, f)) %>% 
  unnest() %>% 
  mutate(antal_tiles = y^2 - i^2) %>% 
  filter(antal_tiles < l+1) %>% 
  nrow()




mutate(i = map2(i_min, i_max, f)) %>% 
  unnest() %>% 
  mutate(antal_tiles = y^2-i^2) %>% 
  View()





