191

library(tidyverse)
library(stringr)
muligheder <- c("L", "O", "A")
m5 <- expand.grid(muligheder, muligheder, muligheder, muligheder, muligheder, stringsAsFactors = F) %>% 
  transmute(m5 = str_c(Var1, Var2, Var3, Var4, Var5))

m5 <- m5 %>% 
  filter(!str_detect(m5, "AAA")) %>% 
  filter(str_count(m5, "L")<2) %>% 
  .$m5

m10 <- expand.grid(m5,m5 , stringsAsFactors = F)

m10 <- m10 %>% 
  transmute(streng = str_c(Var1, Var2)) %>% 
  filter(!str_detect(streng, "AAA")) %>% 
  filter(str_count(streng, "L")<2) %>% 
  .$streng

m15 <- expand.grid(m10, m5, stringsAsFactors = F)

m15 <- m15 %>% 
  transmute(streng = str_c(Var1, Var2)) %>% 
  filter(!str_detect(streng, "AAA")) %>% 
  filter(str_count(streng, "L")<2) %>% 
  .$streng

m150 <- m15 %>% 
  enframe(name=NULL) %>% 
  filter(str_count(value, "L")==0) %>% 
  .$value

m151 <- m15 %>% 
  enframe(name=NULL) %>% 
  filter(str_count(value, "L")==1) %>% 
  .$value

der er nogen der starter med A eller AA.
Der er nogen der slutter med A eller AA

Vi hægter to strenge fra m15 sammen. 
De der slutter med AA kan ikke hægtes sammen med nogen der starter med A
Der der slutter med A kan ikke hægtes sammen med de der starter med AA

Ud fra det her, bør det være muligt at etablere et antal kombinationer, der tilsammen giver det ønskede svar.

m150 %>% 
  enframe(name=NULL) %>% 
  mutate(start1 = str_sub(value, 1,1)) %>% 
  mutate(start2 = str_sub(value, 1,2)) %>% 
  filter(start1 !="A") %>% 
  filter(start2 != "AA")

kombinationsmulighederne afhænger af hvad strengene starter og slutter med, og hvor mange L der er i den i forvejen.

m150 har tre muligheder.

De kan slutte med A (men ikke AA). Dem er der 3136 af
De kan slutte med AA. Dem er der 1705 af.
Og de kan slutte med noget andet end A i de sidste to positioner. Dem er der 5768 af

m151 har de samme tre muligheder.
A, men ikke AA 27820
AA 14071
ingen A i de to sidste positioner 54736

m151 har 
96627 ialt
14071 der starter med AA
27802 der starter med A (men ikke AA)
54736 der hverken starter med A eller AA

m150 har 
10609 ialt
1705 der starter med AA
3136 der starter med A (men ikke AA)
5768 der hverken starter med A eller AA


m150 der slutter med A (men ikke AA) (3136), kan kombineres med alle, bortset fra dem der starter med AA (10609-1705+96627-14071)
m150 der slutter med AA (1705), kan kombineres med alle, der hverken starter med A eller AA (5768+54736)
m150 der hverken slutter med A eller AA (5768) kan kombineres med alt (m150+m151) (10609+96627) 

m151 der slutter med A (men ikke AA) (27820) kan kombineres med alle fra m150, bortset fra dem der starter med AA (10609-1705)
m151 der slutter med AA, (14071) kan kombineres med alle fra m150, bortset fra dem der starter med A eller AA (5768)
m151 der hverken slutter med A eller AA (54736) kan kombineres med alt fra m150 (10609) 

=============
(3136)* (10609-1705+96627-14071) + (1705)* (5768+54736) +
 (5768) * (10609+96627) +
(27820) * (10609-1705)+
(14071) * (5768)+
 (54736) * (10609) 

