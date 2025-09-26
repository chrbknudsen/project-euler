Et S tal er et naturligt tal, der er et perfekt kvadrattal.
Og hvor vi kan få dets kvadratrod ved at splitte det op i et eller flere tal (81 -> 81, 45687 -> 45, 687 eller 4, 5, 68, 7)
og summere dem. 

Så vi skal have fundet en måde at lave alle mulige opsplitninger af et tal.


install.packages("partitions")
library(partitions)
partitions::riffle()

n <- 4
cuts <- unlist(lapply(0:(n-1), function(k) combn(1:(n-1), k, simplify = FALSE)), recursive = FALSE)
cuts
combn(1:3, 2)
