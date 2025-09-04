is_triangle <- function(x) {
  if (x < 1) return(FALSE)
  s <- sqrt(8 * x + 1)
  s == floor(s)
}

triangle <- function(x){
  (x*(x+1))/2
}

hexagonal <-function(x){
  2*x^2 - x
}

is_pentagonal <- function(x){
  if(x<1) return(FALSE)
  s <- (sqrt(24*x +1)+1)/6
  s == floor(s) # We probably need some way of controlling the precision
}