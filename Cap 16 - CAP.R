rm(list = ls())

# Procedimentos para calcular a CAP no software R

## Exemplo 1
## Dados de abundância de espécies como matriz resposta e uma
# matriz de variáveis abióticas (variável contínua)

library(vegan) # carregar o pacote
data(varespec) # matriz de dados das espécies
data(varechem) # matriz ambiental

vare.cap <- capscale(varespec ~ N + P + K, varechem, dist = "bray") # rodar #a CAP a partir da distância
# de Bray-Curtis, indicada para dados de abundância
summary(vare.cap) # sintetizando os resultados
plot(vare.cap) # triplot da análise
summary(vare.cap)$constraints # obter os valores dos scores

cor.varechem <- t(cor(summary(vare.cap)$constraints,
                      varechem[,1:3], method = c("spearman"))) # correlação entre os
# eixos das CAP com algumas variáveis ambientais
cor.varechem

anova.cca(vare.cap) # testar a significância global da análise

## Exemplo 2
## Dados de abundância de espécies e a segunda matriz composta
# por uma variável categórica (grupo ou fator)
library(vegan) # carregar o pacote
data(dune) # matriz de espécies
data(dune.env) # variável categórica - no caso, será utilizada
# a coluna 'Management'

dune.cap <- capscale(dune ~ Management, dune.env, dist = "bray") # rodar a análise baseada na distância de Bray-Curtis e a
# variável categórica 'Management'
summary(dune.cap) # sintetizando os resultados
plot(dune.cap) # triplot

library(BiodiversityR) # carregar o pacote - classificar os
# sites de acordo com a variável categórica
plot.dune.cap <- ordiplot(dune.cap)
ordisymbol(plot.dune.cap, dune.env, "Management", legend = TRUE)

# boxplots dos escores dos eixos para verificar as diferenças
# entre as variáveis categóricas
escores.dune.cap <- cbind.data.frame(summary(dune.cap)$constraints[,1:3], dune.env$Management)
boxplot(CAP1 ~ dune.env$Management, data = escores.dune.cap, ylab = "CAP1", col = "lightgray")
boxplot(CAP2 ~ dune.env$Management, data = escores.dune.cap, ylab = "CAP2", col = "lightgray")
boxplot(CAP3 ~ dune.env$Management, data = escores.dune.cap, ylab = "CAP3", col = "lightgray")

cor.dune.cap <- t(cor(summary(dune.cap)$constraints[,1:3], dune, method = c("spearman"))) #correlações de Spearman entre os
# eixos da CAP e cada variável da matriz resposta (dune)
cor.dune.cap
