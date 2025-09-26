sum(0:9)
library(gmp)
end <- as.bigz("999999999999999999")
start <- as.bigz("899999999999999999")


v <- seq(from = start, to = end, by = 1)


as.character(d_18) |> 
  strsplit("") |> 
  lapply(sort) |> 
  lapply(paste0, collapse = "") |> 
  lapply(as.numeric) |> 
  unlist() |> 
  sum()

as.bigz("999999999999999999")


vi kan glemme alle nullerne. Så der er fiksere måder at kombinere det på.

Det første ciffer kan være 0-9. Det kan det andet også.

der er noget med noget kombinatorik der kan hjælpe os.


