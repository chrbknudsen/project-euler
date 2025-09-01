is_triangle <- function(x) {
  if (x < 1) return(FALSE)
  s <- sqrt(8 * x + 1)
  s == floor(s)
}

triangle <- function(x){
  (x*(x+1))/2
}