# 91


i <- 50
expand.grid(0:i,0:i,0:i,0:i)  %>% 
  rename(x1= Var1, y1 = Var2, x2 = Var3, y2=Var4) %>% 
  filter(!(x1 == 0 & y1 == 0)) %>% 
  filter(!(x2 == 0 & y2 == 0)) %>% 
  filter(!(x1==x2 & y1 == y2)) %>% 
  mutate(PQ2 = (x2-x1)^2 + (y2-y1)^2) %>% 
  mutate(OP2 = x1^2 + y1^2) %>% 
  mutate(OQ2 = x2^2 + y2^2) %>% 
  mutate(h1 = !as.logical(PQ2 - OP2 - OQ2)) %>% 
  mutate(h2 = !as.logical(OP2 - PQ2 - OQ2)) %>% 
  mutate(h3 = !as.logical(OQ2 - PQ2 - OP2)) %>% 
  filter(h1|h2|h3) %>% 
  nrow()/2

