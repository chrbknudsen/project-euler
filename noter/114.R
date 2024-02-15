114

Vi har 50 positioner. En eller flere røde klodser, der hver dækker mindst 3 positioner, skal placeres.
der skal være mindst en tom position mellem hver af de røde klodser.

Hvor mange måder kan det gøres på?
  
  Har vi en rød klods, med længden 50, kan den placeres på en måde.

Har vi en rød klods, med længden 49 kan det gøres på to måder. En grå klods enten før eller efter.

Er den 48 har vi to grå klodser at gøre godt med. 
De kan placeres som 2,0, 1,1 0,2 altså 2, 1 eller 0 før.

så hvis den røde klods har længden k, kan det gøres på (50-k)+1 måde.

Så er der en klods:
  
sum(1:48)
En rød klods, af længde mellem 50 og 47 kan placeres på 1176 måder.

Har vi to. Der skal være mindst en imellem.

Summen af de røde klodser går derfor mellem 49 og 6. 

Er summen 49, kan den første være 3:46. De øvrige måder at placere dem på er givet ud fra den første klods.

så for sum 49: length(3:46) 44. måder


Er summen 48, kan den første klods være mellem length(3:45).  43. der er så to grå klodser. nr. to kan placeres på tre måder.

Er summen 47, kan den første klods være mellem 3:44. 42 måder. Der er nu tre grå klodser. De kan placeres 




length(3:45)

Der er to grå klodser. 
Der skal være en mellem de to røde. Og den anden kan så, for hver konfiguration placeres på tre måder.
