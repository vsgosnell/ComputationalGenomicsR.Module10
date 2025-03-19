
# phylogenetic tree

install.packages("ape")
library("ape")
plot(as.phylo(hc), type = "fan")

#change the margin
par(mar = c(0,0,0,0))
plot(as.phylo(hc), type = "fan")

