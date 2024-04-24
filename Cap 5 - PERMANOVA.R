rm(list = ls())

# Procedimentos para calcular PERMANOVA para um fator no programa R

library(vegan)

TEMP <- c(28, 28.9, 28.7, 29.3, 28.6, 28.8, 29.3, 28.1, 29.5)

OD <- c(2.52, 2.82, 4.26, 3.87, 2.18, 2.48, 0.37, 1.9, 0.18)

pH <- c(6.23, 6.42, 6.23, 6.63, 6.31, 6.79, 6.57, 6.47, 6.37)

COND <- c(34.2, 35.2, 27.6, 32.1, 44.1, 42.3, 64.8, 41.7, 50.9)

dados <- cbind(TEMP, OD, pH, COND) # Une os 4 vetores em uma matriz

dadosp <- scale(dados) # Padronização dos dados em valores de Z 

grupos <- factor(c(rep(1, 3), rep(2, 3), rep(3, 3)), labels = c("um", "dois", "tres")) # Cria um fator com 3 tratamentos ("um", "dois" e "tres") e 3 repetições "rep" de cada tratamento

distancia <- vegdist(dadosp, m = "eu") # Gera a matriz de distância dos dados abióticos pelo método 'euclidean' (distância euclidiana)

homogeneidade <- betadisper(distancia, grupos) # Calcula a homogeneidade das dispersões multivariadas

(testehomog <- permutest(homogeneidade, pairwise = TRUE)) # Testa a homogeneidade das dispersões multivariadas. 'pairwise=TRUE' permite comparações par a par sejam feitas

(permanova <- adonis2(distancia ~ grupos, permutations = 999)) # Faz o cálculo da Permanova
