rm(list = ls())

# Procedimentos para calcular Teste de Mantel no programa R

library(vegan) # Carregar o pacote 'vegan' para realizar as análises

# Se quiser rodar com dados do R
data(varespec)
x <- varespec

data(varechem)
y <- varechem

x.dist <- vegdist(x, method = "bray") # Um exemplo de matriz de dissimilaridade pelo método Bray-Curtis

y.dist <- vegdist(y, method = "euclidean") # Um exemplo de matriz de distância com método euclidiano

mantel(x.dist, y.dist, method = "pearson", permutations = 999)
