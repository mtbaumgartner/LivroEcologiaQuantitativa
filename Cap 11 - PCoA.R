rm(list = ls())

# Procedimentos para calcular PCoA no programa R

library(vegan)
library(ape)

data(varespec)  # Chamar a planilha de dados do R

fator <- as.factor(c(rep("se", 13), rep("ba", 11))) 
# Criar um fator referente ao habitat com dois níveis: "se" e "ba". 
# Este vetor será usado para fazer um teste de hipótese entre esses dois níveis ("se" e "ba") com os eixos resultantes da PCoA, ao final deste script

# A PCoA pode ser realizada por três comandos diferentes: 'cmdscale' (pacote: stats), 'pcoa' e 'dudi.pco' (pacote: ade4)

##### Script cmdscale #####

sppbray <- vegdist(varespec, "bray")   
# Transformar a matriz de dados original em uma matriz de dissimilaridade de Bray-Curtis
# Se for uma planilha de presença-ausência, usar a dissimilaridade de Jaccard

pcoa1 <- cmdscale(sppbray, k = (nrow(varespec) - 1), eig = T)  # Comando para PCoA
# 'k' = número de dimensões da análise, equivale ao número de eixos que você quer usar, esta escolha pode ser arbitrária, por exemplo: k = 2
# 'eig = T': para mostrar os autovalores no output 
# 'add = T: para usar um método de correção dos autovalores negativos

pcoa1  # Visualizar o resultado
# A porcentagem de explicação dos eixos escolhidos pela função 'cmdscale' é o primeiro valor do GOF 

### Plotar o gráfico:

plot(pcoa1$points[, 1:2], type = "n", xlab = "PCoA 1", ylab = "PCoA 2")  # Gráfico vazio
# Locais como símbolos distintos:
points(scores(pcoa1)[1:13, 1], scores(pcoa1)[1:13, 2], pch = 16, col = "blue", cex = 1.5)
points(scores(pcoa1)[14:24, 1], scores(pcoa1)[14:24, 2], pch = 17, col = "red", cex = 1.5)
legend("topright", legend = c("SE", "BA"), pch = c(16, 17), col = c("blue", "red"))  # Legenda

### Se quiser extrair os autovetores e autovalores:

pcoa1$points[, 1:2]
pcoa1$eig

### Se desejar exportar como .csv
write.table(pcoa1$points[, 1:2], "PCoA_1_2.csv")
write.table(pcoa1$eig, "PCoA_eigen.csv")

##### Teste de hipótese utilizando os escores dos eixos:

# A partir dos eixos da PCoA, pode-se fazer um teste de hipóteses.
# Aqui, foi realizada uma ANOVA entre os dois níveis ("se" e "ba") do fator criado acima

anova.eixo1 <- aov(pcoa1$points[, 1] ~ fator)  # ANOVA entre os scores do primeiro eixo, utilizando o vetor fator para definir os níveis
summary(anova.eixo1)

anova.eixo2 <- aov(pcoa1$points[, 2] ~ fator)  # ANOVA entre os scores do segundo eixo, utilizando o mesmo vetor para definir os níveis
summary(anova.eixo2)
