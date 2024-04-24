rm(list = ls())

# Procedimentos para calcular PCA no programa R

library(vegan) # Carrega o pacote

cond <- c(2019, 379, 367, 1293, 1970, 841, 167, 62, 1342, 256, 94, 223,
          147, 301, 384, 253, 53.7, 69.28, 45, 121.44, 98.76, 107.3, 110.24, 148.7, 151.03)

alcal <- c(143, 114, 158, 150, 135, 137, 10, 16, 128, 158, 49, 131, 75,
           109, 179, 97, 19.2, 26.5, 23.91, 33.5, 31, 28.75, 34.25, 25, 38.5)

ninor <- c(60, 70, 70, 80, 90, 90, 140, 690, 160, 320, 330, 380, 550,
           580, 600, 740, 205.02, 266.66, 403.76, 484.55, 490.12, 575.05, 648.01, 651.52, 951.66)

ntot <- c(630, 440, 620, 1200, 630, 525, 630, 870, 600, 510, 610,
          530, 900, 940, 780, 1130, 517.92, 598.96, 1025.2, 870.08, 684.2, 1122.14, 1051.46, 1340.44, 2171.38)

porto <- c(10, 8, 9, 8, 7, 9, 8, 18, 5, 96, 6, 4, 16, 31, 4, 38, 16.96,
           19.35, 12.64, 9.32, 10.06, 17.79, 12.18, 34.75, 26.3)

ptot <- c(41, 20, 37, 36, 22, 46, 45, 42, 15, 136, 22, 15, 39, 59, 10,
          87, 21.82, 23.75, 22.96, 26.59, 22.28, 44.64, 21.23, 67.11, 83.36)

Y <- cbind(cond, alcal, ninor, ntot, porto, ptot) # O comando 'cbind' junta os seis vetores em uma nova matriz com todas as variáveis

fator <- as.factor(c("C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
                     "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z"))
cor(Y)

boxplot(Y)

YP <- scale(Y)

boxplot(YP)

pca <- prcomp(YP) # PCA usando os dados padronizados 

summary(pca) # Mostra a porcentagem de variância capturada por cada eixo

escores <- scores(pca) # Extrai os escores das amostras

(autovalores <- apply(escores, 2, var))
# Procedimentos para calcular PCA no programa R

library(vegan) # Carrega o pacote
cond <- c(2019, 379, 367, 1293, 1970, 841, 167, 62, 1342, 256, 94, 223,
          147, 301, 384, 253, 53.7, 69.28, 45, 121.44, 98.76, 107.3, 110.24, 148.7, 151.03)
alcal <- c(143, 114, 158, 150, 135, 137, 10, 16, 128, 158, 49, 131, 75,
           109, 179, 97, 19.2, 26.5, 23.91, 33.5, 31, 28.75, 34.25, 25, 38.5)
ninor <- c(60, 70, 70, 80, 90, 90, 140, 690, 160, 320, 330, 380, 550,
           580, 600, 740, 205.02, 266.66, 403.76, 484.55, 490.12, 575.05, 648.01, 651.52, 951.66)
ntot <- c(630, 440, 620, 1200, 630, 525, 630, 870, 600, 510, 610,
          530, 900, 940, 780, 1130, 517.92, 598.96, 1025.2, 870.08, 684.2, 1122.14, 1051.46, 1340.44, 2171.38)
porto <- c(10, 8, 9, 8, 7, 9, 8, 18, 5, 96, 6, 4, 16, 31, 4, 38, 16.96,
           19.35, 12.64, 9.32, 10.06, 17.79, 12.18, 34.75, 26.3)
ptot <- c(41, 20, 37, 36, 22, 46, 45, 42, 15, 136, 22, 15, 39, 59, 10,
          87, 21.82, 23.75, 22.96, 26.59, 22.28, 44.64, 21.23, 67.11, 83.36)
Y <- cbind(cond, alcal, ninor, ntot, porto, ptot) # O comando 'cbind' junta os seis vetores em uma nova matriz com todas as variáveis
fator <- as.factor(c("C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z"))
cor(Y)
boxplot(Y)
YP <- scale(Y)
boxplot(YP)

pca <- prcomp(YP) # PCA usando os dados padronizados 
summary(pca) # Mostra a porcentagem de variância capturada por cada eixo

escores <- scores(pca) # Extrai os escores das amostras
autovalores <- apply(escores, 2, var)
(autovalores / sum(autovalores)) * 100 # Mostra a porcentagem de explicação dos eixos

bstick(pca)

(autovetores <- t(t(pca$rotation) * pca$sdev)) # Essa multiplicação devolve os autovetores à escala original

biplot(pca) # Gráfico inicial da PCA 

plot(escores[, 1:2], type = "n", ylim = c(-3.2, 1.8), xlim = c(-2.7, 4.7)) # Plota um gráfico vazio
# Países como símbolos distintos:
points(escores[1:16, 1], escores[1:16, 2], pch = 16, cex = 1.5)
points(escores[17:25, 1], escores[17:25, 2], pch = 6, cex = 1.5)
legend("topright", c("EUA", "Brasil"), pch = c(16, 6), bty = "n")
# Setas indicando as variáveis mais representativas em cada eixo:
legend("bottomright", legend = c("Ninor", "Ntot"), bty = "n", y.intersp = 1) # Insere os nomes das variáveis
arrows(3.4, -3.1, 3.9, -3.1) # Insere seta
legend("bottomleft", legend = c("Ptot", "Porto"), bty = "n", y.intersp = 1) # Insere os nomes das variáveis
arrows(-2.5, -2.4, -2.5, -2.9) # Insere seta

anova.eixo1 <- aov(escores[, 1] ~ fator) # Selecionamos a primeira coluna dos escores e testamos com o fator
summary(anova.eixo1)
#            Df Sum Sq Mean Sq F value Pr(>F)  
# fator      1  14.09  14.090   6.394 0.018

(autovalores / sum(autovalores)) * 100 # Mostra a porcentagem de explicação dos eixos

bstick(pca)

(autovetores <- t(t(pca$rotation) * pca$sdev)) # Essa multiplicação devolve os autovetores à escala original

biplot(pca) # Gráfico inicial da PCA 

plot(escores[, 1:2], type = "n", ylim = c(-3.2, 1.8), xlim = c(-2.7, 4.7)) # Plota um gráfico vazio
# Países como símbolos distintos:
points(escores[1:16, 1], escores[1:16, 2], pch = 16, cex = 1.5)
points(escores[17:25, 1], escores[17:25, 2], pch = 6, cex = 1.5)
legend("topright", c("EUA", "Brasil"), pch = c(16, 6), bty = "n")
# Setas indicando as variáveis mais representativas em cada eixo:
legend("bottomright", legend = c("Ninor", "Ntot"), bty = "n", y.intersp = 1) # Insere os nomes das variáveis
arrows(3.4, -3.1, 3.9, -3.1) # Insere seta
legend("bottomleft", legend = c("Ptot", "Porto"), bty = "n", y.intersp = 1) # Insere os nomes das variáveis
arrows(-2.5, -2.4, -2.5, -2.9) # Insere seta

anova.eixo1 <- aov(escores[, 1] ~ fator) # Selecionamos a primeira coluna dos escores e testamos com o fator
summary(anova.eixo1)
#            Df Sum Sq Mean Sq F value Pr(>F)  
# fator      1  14.09  14.090   6.394 0.018
