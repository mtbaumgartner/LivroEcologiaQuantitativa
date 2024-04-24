rm(list = ls())

# Procedimentos para calcular CA e DCA no programa R

# DADOS BCI, COMPOSIÇÃO (ABUNDÂNCIA), AMBIENTAIS E ESPACIAIS

library(vegan)
library(BiodiversityR)

data(BCI)
data(BCI.env)

# Carregar e visualizar bancos de dados BCI e BCI.env
data("BCI", "BCI.env")
head(BCI)
dim(BCI)
dim(BCI.env)

# Na distribuição espacial das parcelas, pode-se observar um grandiente espacial horizontal, o qual pode-se assumir uma depenência espacial na composição
BCI.parcelas <- plot(BCI.env$UTM.EW, BCI.env$UTM.NS, 
                     type = "p", ylab = "UTM.NS", xlab = "UTM.EW")
text(as.numeric(BCI.env$UTM.EW) + 20, BCI.env$UTM.NS,
     labels = c(row.names(BCI.env)))
arrows(625800, 1011800, 626600, 1011800, lty = 1, col = 4, lwd = 5)
abline(v = c(625920, 626120, 626320, 626520, 626720), lty = 2, col = "red")

# Criando fatores espaciais
BCI.fator <- as.factor(rep(1:5, each = 10))
BCI.fator

# CA
ca1 <- cca(BCI)  
ca1

names(ca1)
ca1$tot.chi # inércia

names(ca1$CA)
autovalores <- ca1$CA$eig

scores.locais <- ca1$CA$u
head(scores.locais)

scores.sp <- ca1$CA$v
head(scores.sp)

plot(ca1)
plot(ca1, display = "sites")

# DCA  
dca1 <- decorana(BCI)
dca1
plot(dca1) 

# DCA com peso menor para espécies raras
dca2 <- decorana(BCI, iweigh = 1)  

# Proporção de explicação dos eixos
perc.exp <- dca1$evals / ca1$tot.chi
perc.exp

# Primeiro vamos plotar um gráfico vazio
plot(dca1, disp = "sites", type = "n")

# Plota-se os locais como símbolos distintos
points(scores(dca1)[1:10, 1], scores(dca1)[1:10, 2], 
       pch = 16, col = "red", cex = 1.5)
points(scores(dca1)[11:20, 1], scores(dca1)[11:20, 2], 
       pch = 17, col = "blue", cex = 1.5)
points(scores(dca1)[21:30, 1], scores(dca1)[21:30, 2], 
       pch = 23, col = "green4", bg = "green4", cex = 1.5) 
points(scores(dca1)[31:40, 1], scores(dca1)[31:40, 2], 
       pch = 16, col = "black", cex = 1.5)
points(scores(dca1)[41:50, 1], scores(dca1)[41:50, 2], 
       pch = 15, col = "magenta", cex = 1.5)

# Plotar o Mínimo Polígono Convexo
ordihull(dca1, BCI.fator, col = "dimgray", lwd = 1.5, lty = 2)

# Plotar os centroides dos grupos
dca.fit <- envfit(dca1 ~ BCI.fator)
dca.fit
plot(dca.fit, col = "black", labels = c("1", "2", "3", "4", "5"), cex = 2)

# Plotar o elipsóide de erro padrão
ordiellipse(dca1, BCI.fator, kind = "se", conf = 0.95, lwd = 1, 
            draw = "polygon", col = "grey", border = "black", alpha = 80)

# Plotar a legenda
legend("topright", c("Fator1", "Fator2", "Fator3", "Fator4", "Fator5"), 
       pch = c(16, 17, 18, 16, 15), col = c("red", "blue", "green4", "black", "magenta"), bty = "n")

# Teste de hipótese (ANOVA) utilizando o score dos eixos
anova.dca1 <- aov(scores(dca1)[, 1] ~ BCI.fator)
summary(anova.dca1)
Tukey.dca1 <- TukeyHSD(anova.dca1, ordered = TRUE)
Tukey.dca1

anova.dca2 <- aov(scores(dca1)[, 2] ~ BCI.fator)
summary(anova.dca2)
Tukey.dca2 <- TukeyHSD(anova.dca2, ordered = TRUE)
Tukey.dca2
