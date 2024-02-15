94

Almost equilateral triangles.
It is easily proved that no equilateral triangle exists with integral length sides and integral area. However, the almost 
equilateral triangle 5-5-6 has an area of 12 square units.

We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs 
by no more than one unit.

Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose perimeters do 
not exceed one billion (1,000,000,000).

a, a, a+1

arealet skal også findes.

arealet. Sådan en ligebenet trekant har tre sider.
Den kan deles op i to retvinklede trekanter, hvor hypotenusen er a
Den ene katete er (a+1)/2
Og den anden katete kan findes udfra:
  a^2 + b^2 = c^2

Eller:
  ((a+1)/2)^2 + b^2 = a^2

<=> 
  b^2 = a^2 - ((a+1)/2)^2
<=>
  b = sqrt(a^2 - ((a+1)/2)^2)

Arealet gives så ved:
  (a+1)/2 * b * 1/2

Og da der er to trekanter, bliver det samlede areal:
  (a+1)/2 * b

Eller udtrykt alene i a:
  
  a <- c(1,2,3,4,5)
  (a+1)/2 * sqrt(a^2 - ((a+1)/2)^2)
  

test <- 1:333333333
object.size(test)



A <- function(a){
  (a+1)/2 * sqrt(a^2 - ((a+1)/2)^2)
}
options(scipen=999)
A(333)

Og det er jo fint. Men. det er pænt mange værdier der skal testes. Der må være et eller andet, hvor vi kan finde heltallige løsninger.

(a+1)/2 * sqrt(a^2 - ((a+1)/2)^2)


a <- 5

3*a^4 + 4*a^3 - a^2 -4*a -1

A(test)

a <- 5

25-8+1
