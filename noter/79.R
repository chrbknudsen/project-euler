#79

library(dplyr)
library(tibble)

tallene <- c(319, 680,180,690,129,620,762,689,762,318,368,710,720,710,629,168,160,689,716,731,736,729,316,729,729,710,769,290,719,680,318,389,
162,289,162,718,729,319,790,680,890,362,319,760,316,729,380,319,728,716)




tallene <- tallene %>% 
  unique() %>% 
  enframe() %>% 
  mutate(first = value%/%100) %>% 
  mutate(second=value%/%10%%10) %>% 
  mutate(third = value%%10) 

tallene

unique(tallene$third[!tallene$third %in% c(tallene$first, tallene$second)])

dvs at der er et tal, på sidste position, der ikke optræder nogen andre steder. Det sidste ciffer i koden er 0.

unique(tallene$first[!tallene$first %in% c(tallene$third, tallene$second)])

der er et tal, i første position, der kun optræder i første position. Det første ciffer i koden er 7.

Det andet ciffer i koden må være blandt forsøgene. Det kan ikke være 7. Det skal findes blandt de cifre, der er i første position i et af forsøgene.
Men ikke er 7. Og blandt dem der er i anden position, hvor første ciffer er 7. Hvor mange er der at vælge imellem?
  
sort(unique(c(tallene$first, tallene$second, tallene$third)))
Det er dem vi har at gøre godt med. 

Er der gentagne cifre?

tallene %>% 
  rowwise() %>% 
  mutate(gent = length(unique(c(first,second,third)))) %>% 
  ungroup() %>% 
  summarize(test = sum(as.numeric(gent!=3)))

nej, det har vi ingen evidens for. 

OK. Vi kender positionen af 7 og 0 (hhv først og sidst).

Følgende cifre skal vi så have fundet positionerne af:
  1,2,3,6,8,9
    
Hvilke cifre kommer før 1?
tallene %>% 
  filter(second==1|third==1)

#2?
tallene %>% 
  filter(second==2|third==2)  

#3?
tallene %>% 
  filter(second==3|third==3)  

#6?
tallene %>% 
  filter(second==6|third==6)  

#8?
tallene %>% 
  filter(second==8|third==8)  
#9?
tallene %>% 
  filter(second==9|third==9)  

Kommer før:
  1: 3,7
  2: 1,6,7,3
  3: 7
  6: 1,3, 7
  8: 1,2,3,6,7
  9:1,2, 3,6,7,8,

  Det eneste ciffer der kommer før 3, er 7. 7 er i position 1. så:
    
    73.......0
  1: 3
  2: 1,6,3
  3: 
  6: 1,3, 
  8: 1,2,3,6,
  9:1,2, 3,6,8,
  
  så fjerner vi tre:
  1: 
  2: 1,6,
  6: 1,
  8: 1,2,,6,
  9:1,2, ,6,8,
  
  Næste ciffer er 1: 731
  og vi fjerner 1:
  
  2: 6,
  6: 
  8: 2,,6,
  9:2, ,6,8,

  næste ciffer er så 6: 7316 
  og vi fjerner 6:
    
    2:
  
    8: 2
  9:2, ,8,
  
  Næste ciffer er 2:_ 73162
  Vi fjerner 2:
    
    
    
    8: 
  9: ,8,
  
  næste er 8: 73162890