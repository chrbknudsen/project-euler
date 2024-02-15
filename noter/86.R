Euler 86


# A spider, S, sits in one corner of a cuboid room, measuring 6 by 5 by 3, and a fly, F, 
# sits in the opposite corner. 
# By travelling on the surfaces of the room the shortest "straight line" distance from S to 
# F is 10 and the path is shown 
# on the diagram.
# 
# 
# However, there are up to three "shortest" path candidates for any given cuboid and the 
# shortest route doesn't always have 
# integer length.
# 
# It can be shown that there are exactly 2060 distinct cuboids, ignoring rotations, with integer 
# dimensions, up to a maximum 
# size of M by M by M, for which the shortest route has integer length when M = 100. 
# This is the least value of M for which 
# the number of solutions first exceeds two thousand; the number of solutions when M = 99 is 1975.
# 
# Find the least value of M such that the number of solutions first exceeds one million.
# 
# Fluen kan tage tre ruter, a, b og c.
# langs hhv gulvet, gavlen og væggen.
# Ruten er defineret ved værdien af a, b eller c.
# 
# Kuboiden er defineret ved tre længder, l,b,h

library(dplyr)
library(stringr)
# OK. Vi folder kassen ud. Og får en langt lettere opgave at løse.
# 
# Det kan vi gøre på tre måder.
# 
# dist^2 = h^2 + (b+l)^2
# 
# dist^2 = l^2 + (h+b)^2
# 
# og
# 
# dist^2 = b^2 + (h+l)^2
# 
# Når vi udvider disse, får vi
# 
# d = h^2 + b^2 + l^2 + 2bl
# d = l^2 + h^2 + b^2 + 2hb
# d = b^2 + h^2 + l^2 + 2hl
# 
# Med andre ord, den korteste rute finder vi der, hvor
# bl, hb og hl er mindst.
# Hvis vi lægger begrænsninger på siderne:
#   l >= b >= h
# Så vil 
# bh altid være mindre end (eller lig) lb
# hl vil altid være større (eller lig) bh
# hl >= bh
# Så det vil aldrig være hl der er den korteste.
# Og bh vil altid være mindre end eller lig lb 
# så det er bh udgaven der altid vil være den korteste.
# Så, hvis l>= b >= h, så har vi altid at den korteste distance er:
#   
#   d = l^2 + h^2 + b^2 + 2hb
# eller: 
#   dist^2 = l^2 + (h+b)^2

# Vi kan generere alle unikke pytagoræiske tripler ved:
#   
# a = k * (m^2 - n^2)
# b = 2mnk
# c = k * (m^2 + n^2)
# 
# where m, n, and k are positive integers with m > n, and with m and n coprime and 
# not both odd

solutions <- list()

M <- 100
T <- 99
for(k in 1:M){
  for(m in 2:M){
    for(n in 1:(m-1)){
      a <- k*(m*m - n*n)
      b <- 2*m*n*k
      c <- k*(m*m + n*n)
      if(m > n){
        solutions[[ (length(solutions) + 1) ]] <- c(a=a, b=b, c=c, k=k, m=m,n=n)
        
      }
    }
  }
}

View(solutions)
# 
# a=k\cdot (m^{2}-n^{2}),\ \,b=k\cdot (2mn),\ \,c=k\cdot (m^{2}+n^{2})
# 
# Så hvilke pythagoræiske tripler a,b,c har vi, hvor 
# M <- 300
# expand.grid(h=1:M, b=1:M, l=1:M) %>% 
#   mutate(r1 = h^2+ (b+l)^2,
#          r2 = b^2 + (h+l)^2,
#          r3 = l^2 + (b+h)^2) %>% 
#   mutate(r1 = sqrt(r1),
#          r2 = sqrt(r2),
#          r3 = sqrt(r3)) %>% 
#   mutate(kortest = pmin(r1,r2,r3)) %>% 
#   mutate(h1 = pmin(h,b,l)) %>% 
#   mutate(h2 = pmax(h,b,l)) %>% 
#   mutate(h3 = h+b+l-h1-h2) %>% 
#   mutate(H = str_c(h1, h2, h3, sep="-")) %>% 
#   filter(near(0, kortest%%1)) %>%
#   distinct(H) %>% 
#   View()
# 
# 
# 11403/6
# 
# 216.9101 %% 1
# 
#     ?pmin
# 
# Distancen kan max være 
# d_max^2 = (M^2 + (2*M)^2 =
#         = M^2 + 4*M^2  =
#         = 5* M^2
# d_max   = sqrt(5)*M^2
# 
# sqrt(5)
# 
# alle primitive pythagoræiske tripler, med c under sqrt(5)*M^2
# 
# a=m^{2}-n^{2}, b=2mn,c=m^{2}+n^{2}
# 
# Med begrænsninger på n og m.
# 
# Men der har vi så også at 
# c = m^2 + n^2 max kan være sqrt(5)*M^2
# 
# dvs at m allerhøjest kan være
# m^2 = 2.3*M^2
# eller
# 1/sqrt(5)
# 5^(-1/2) * m^2 = M^2
# 
# (5^(-1/2))^(1/2)
# 
# så vi søger m op til 0.7*M
# 
# m<- 2
# n <- 1
# 
# pyt <- function(n,m){
#   a <- m^2-n^2
#   b <- 2*m*n
#   c <- m^2+n^2
#   return(c(a,b,c))
# }
# 
# 
# pyt(2,4)/4
# pyt(1,3)
# Nu har vi så oplysningen at 
# l = 6 og b+h=8
# har en heltallig løsning.
# Så skal vi blot finde 
# 
# 
# 
# Se, vi kender b,h og l
# 
# Hvilke af de tre giver et minimum, der er heltalligt?
#   
#   Ved M = 100 er de 2060 kombinationer af b,h,l, der giver en kortest rute, der er heltallig.
# 
# b,h,l <- 1:100
# 
# 100*100*100
# 
# Der er 1.000.000 mulige kombinationer.
# 
# For det er retvinklede trekanter. Find alle heltallige 
# 
# kandi <- 
# 
# kand
# 
# library(dplyr)
# library(tidyverse)
# map
# kand <- expand.grid(1:200,1:200,1:200) %>% 
#   mutate(v1 = pmap(list(Var1, Var2, Var3), list))  %>% 
#   mutate(v2 = unlist(map(v1, function(x) paste(as.character(sort(as.numeric(unlist(x)))), collapse="-"))))
# 
# kand <- kand  %>% 
#   select(Var1, Var2, Var3, v2) %>% 
#   group_by(v2) %>% 
#   slice(1) %>% 
#   ungroup()
#   
#   
# 
# kand <- kand %>% 
#   mutate(d1 = sqrt(Var1^2 + (Var2+Var3)^2)) %>% 
#   mutate(d2 = sqrt(Var2^2 + (Var1+Var3)^2)) %>%
#   mutate(d3 = sqrt(Var3^2 + (Var2+Var1)^2)) %>% 
#   gather(key = rute, value= dist, d1,d2,d3) 
# 
# kand <- kand %>% 
#   group_by(v2) %>% 
#   mutate(kortest = min(dist)) %>% 
#   select(v2, kortest) %>% 
#   distinct()
# 
# kand %>% 
#   filter(near(kortest%%1, 0))
# 
# Godt. det er vejen frem. Hvordan skaber jeg så samtlige pythagoræiske tripler med en given max M?
#   
#   det vi ved er, at der hvor den korteste vej er en pythagoræisk trippel, er en løsning. Det kan ikke afvises at der er kortere
# veje gennem kuben end den. I så fald skal den sorteres fra.
# 
# Men hvis jeg kan finde antallet af tripler hvor den længste side er 2*m, så er jeg i nærheden.
# Ikke helt, men det må være vejen frem.
# 
# Sagen er jo - hvis jeg finder en korteste længde hvor side 1 er 10, og side 2+3 er lig 200. Så har jeg i princippet samtlige 
# kombinationer af 1:199 + 199:1 at gøre godt med. Det er 200 men der kan sagtens være flere hvor den kortere vej er kortere.
# 
# 
# 
# kand2 <- kand %>% 
#   mutate(v1 = pmap(list(Var1, Var2, Var3), list))  %>% 
#   mutate(v2 = unlist(map(v1, function(x) paste(as.character(sort(as.numeric(unlist(x)))), collapse="-"))))
# 
# 
# kandi <- kand %>% 
#   ungroup() %>% 
#   select(pid, kortest) %>% 
#   distinct() 
# 
# kandi %>% 
#     filter(near(kortest%%1, 0)) %>% 
#     separate(pid, into = paste("V", 1:3, sep = "")) %>% 
#     mutate(v1 = pmap(list(V1, V2, V3), list))  %>% 
#     mutate(v2 = unlist(map(v1, function(x) paste(as.character(sort(as.numeric(unlist(x)))), collapse="-"))))
# 
# tests %>% 
#   select(kortest, v2) %>% 
#   distinct
#   
# paste(as.character(sort(as.numeric(unlist(tests)))), collapse="-")
# 
# unlist() %>% as.numeric() %>% sort() %>% c() %>% paste0()
# 
# 
# 11403/5
# 
# near(sqrt(2)^2, 2)
# 
# library(tidyverse)
# 
# kandi
#   
# Ruten langs gulvet fører til at distancen langs gulvet er givet ved
# 
# e1^2 = b^2 + a^2
# 
# a er her variabel, og kan tage en hvilken som helst værdi ]0,l]
# Efter at være gået ad gulvet, er den manglende distance givet ved 
# e2^2 = h^2 + (l-a)^2
# 
# Find værdien af a, der minimerer e1+e2
# 
# 
# Og det kan man selvfølgelig.
# Man kan også observere, at hvis man folder kassen ud, går ruten fra S til "F" ad den rette linie SF^2 = 6^2 + (5+3)^2
# 
# Med andre ord, hvilke retvinklede trekanter har en heltallig løsning?
#   
# En tilsvarende overvejelse har vi for de tre andre ruter. 
# Hvis vi kalder de tre sider for l, b og h
# Så er den ene af løsningerne givet ved SF^2 = l^2 + (b+h)^2
# 
# Intet behov for at differentiere.
# 
# 
# 
# sqrt(5^2 + x^2) + sqrt(3^2 + (6-x)^2)
# 
# Hvis vi differentierer den mht x, så får vi:
#   
# 
# 
# h <- 3
# b <- 5
# l <- 6
# f <- function(x) (x/(25 + x^2)) -  (-(x+6)/sqrt(x^2 - 12*x + 45))
# uniroot(f, interval=c(-10, 6))
# f(-4.872527)
# f(6)
# 
# Noget er galt...
# 
# 6^2 + 8^2
# 
# Den kan jeg selvfølgelig godt differentiere. men det giver noget rod, som ikke hjælper skide meget.
# 
# Jeg skal have bedre styr på hvad jeg kalder tingene.
# 
# Men lad os bare se hvordan vi minimerer ruten langs gulvet.
# e1 = c
# 
# e2 = sqrt(h^2 + (l-a)^2)
# 
# sqrt(h^2 + (l-a)^2)+ sqrt(h^2 + (l-a)^2) = afstanden.
# 
# Kan man i stedet minimere
# h^2 + (l-a)^2 +  b^2 + a^2
# ?
#   
# Det giver minimum ved:
#   4a - 2l
# 
# så 4*a - 12 = 0 
# a = 3
# 
# Og det er ikke der minimum ligger.
# 
# Så...
# Og antage at det giver det rette?
#   
#   
# 
# Hvor er den minimal?
#   
#   differentier over a, og se hvor den er 0 for a.
# 
# x/ sqrt()
# 
# 
# e1^2 + e2^2 = b^2 + a^2 + h^2 + (l-a)^2
# 
# 
# 
# fun.1 <- function(a)sqrt(b^2 + a^2) +  sqrt(h^2 + (l-a)^2)
# library(ggplot2)
# p <- ggplot(data = data.frame(x=0), mapping = aes(x=x))
# p + stat_function(fun = f) + xlim(-10,6)
# 
# Hvordan differentiere den funktion?
#   
#   b^2
# ca 3
# 
# a <- 3.8
# a <- a - 0.03
# e1 = sqrt(b^2 + a^2)
# e2 = sqrt(h^2 + (l-a)^2)
# 
# 
# sqrt(34) + sqrt(18)
# 
# e1+e2
# den er ikke integer. 
