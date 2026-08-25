# op til M = 99



generate_cuboids <- function(M){
                      expand.grid(
                        a = 1:M,
                        b = 1:M,
                        c = 1:M
                      ) |>
                      subset(a <= b & b <= c)
                        
}

test <- function(M){

cuboids <- generate_cuboids(1000)


cuboids$ab <- cuboids$a + cuboids$b
cuboids$ac <- cuboids$a + cuboids$c
cuboids$bc <- cuboids$b + cuboids$c

cuboids$ab_c <- cuboids$ab^2 + cuboids$c^2
cuboids$bc_a <- cuboids$bc^2 + cuboids$a^2
cuboids$ac_b <- cuboids$ac^2 + cuboids$b^2




cuboids <- cuboids |> 
  dplyr::mutate(shortest = pmin(ab_c, bc_a, ac_b)) 

cuboids |> 
  dplyr::filter(sqrt(shortest) %% 1 ==0) |> nrow()
}


test(1000)
# det tager simpelthen for lang tid. Det vi snarere har brug for er at 
# kunne generere retvinklede trekanter med heltallige sider.

# det gør vi ved at generere de pythagoræiske tripler.

# Det gør vi ved at vælge to heltal m > n
# de skal være relativt primale, gcd(m,n) = 1
# En skal være ulige, den anden ulige.

triple_a <- numeric(0)
triple_b <- numeric(0)
triple_c <- numeric(0)
for(n in 1:100){
  for(m in (n+1):100){
    if(numbers::GCD(m,n) == 1 & sum(c(m,n) %% 2) == 1){
        triple_a <- c(triple_a, m^2 - n^2)
        triple_b <- c(triple_b, 2*m*n)
        triple_c <- c(triple_c, m^2 + n^2)
    }
  }
}


# sum(c(5,5) %% 2)

# begge lige 0
# en lige, en ulige 1
# begge ulige 2