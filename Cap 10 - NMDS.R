rm(list = ls())

# Procedimentos para calcular a NMDS no programa R

library(vegan)  # O pacote vegan é carregado

data(dune)  # O conjunto de dados a ser utilizado é carregado 

dune.nmds <- metaMDS(dune, distance = "bray", k = 2, display = c("sites", "species"), trymax = 40, try = 20)  # A função que executa o algoritmo da nMDS é executada

summary(dune.nmds)  # O sumário da ordenação é mostrado

plot(dune.nmds)  # O plot da ordenação é construído

# São extraídos os escores dos objetos e das espécies
dune.nmds$points
dune.nmds$species
