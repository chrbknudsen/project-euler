102

A(-340,495), B(-153,-910), C(835,-947)

er 0,0 inden eller uden for denne trekant?
  
  Når vi generelt forsøger at finde ud af om et punkt ligger inden for en polygon, tegner vi en linie fra dette punkt, i en arbitrær 
retning, og tæller hvor mange gange den linie krydser polygonen. Hvis den krydser et ulige antal gange - ja så ligger det indenfor.
Hvis den krydser et ulige antal gange, så ligger den udenfor. 0 sætter vi nok til at være lige.

Hvis vi tegner en linie fra origo til et arbitrært punkt, så kan vi beregne skæringen mellem den og et af liniesegmenterne.
Dernæst finder vi ud af om den skæring ligger inden for liniesegmentet.

Hvordan finder vi skæringen?

  En linie er defineret ved y = a x + b
en anden ved:
  y2 = a2x2 + b2

Den linie vi tester skæring mod er givet ved y = c*x + d
c <- 1
d <- 0

Det er simpelt nok at beregne skæringen mellem to linier.

read_csv("p102_triangles.txt", col_names = F) %>% 
  rename(Ax = X1, Ay = X2, Bx = X3, By = X4, Cx = X5, Cy = X6) %>% 
  mutate(ABa = (Ay-By)/(Ax-Bx)) %>%      # beregner liniens ligning for de tre liniesegmenter, hældning a, skæring b for hhv AB, 
  mutate(ABb = By - ABa*Bx) %>%          # BC og AC
  mutate(BCa = (Cy-By)/(Cx-Bx)) %>% 
  mutate(BCb = By - BCa*Bx) %>% 
  mutate(ACa = (Cy-Ay)/(Ax-Bx)) %>% 
  mutate(ACb = Ay - ACa*Ax) %>% 
  mutate(ABx = (d-ABb)/(ABa-c)) %>%      #beregner skæringspunkter mellem linien der går gennem origo. x og y for hhv AB, BC og AC
  mutate(ABy1 = ABx*c + d) %>% 
  mutate(BCx = (d-BCb)/(BCa-c)) %>% 
  mutate(BCy = BCx*c + d) %>% 
  mutate(ACx = (d-ACb)/(ACa-c)) %>% 
  mutate(ACy = ACx*c + d) %>% 
  View()

BCa har (mindst) en uendelig. Det har ACa også. Og ACb ABx har en NA, ABy1 har en NA, BCx har en NA, BCy har en NA, ACx har en NA
ACy har en NA


BCa optræder ved:
  232	-326	-457	-946	-457	-116
Så når BCa er inf er det fordi BC i dette tilfælde er lodret. 
Så alt det her er meget fint. Men hvad er liniens ligning for en lodret linie?

  
  library(sp)

data <- read_csv("p102_triangles.txt", col_names = F)
data %>% 
  rowwise() %>% 
  mutate(inde = point.in.polygon(0,0, c(X1, X3, X5 ), c(X2, X4, X6))) %>% 
  ungroup() %>% 
  summarise(resultat=sum(inde))
  


point.in.polygon(point.x, point.y, pol.x, pol.y, mode.checked=FALSE)  
