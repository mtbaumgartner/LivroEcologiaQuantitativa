rm(list = ls())

# Procedimentos para calcular PROCRUSTES no programa R

library(ade4)

data(doubs) # Carrega os dados doubs

pca1 <- dudi.pca(doubs$env, scal = TRUE, scann = FALSE) # Ordenação (análise de componentes principais) dos dados ambientais, 'scal=TRUE' corresponde a transformação de Gower aplicada aos dados

pca2 <- dudi.pca(doubs$fish, scal = FALSE, scann = FALSE) # Ordenação (análise de componentes principais) das espécies de peixes

pro3 <- procuste(pca1$tab, pca2$tab, nf = 2) # Análise de Procrustes entre os resultados das ordenações anteriores 

par(mfrow = c(2, 2)) # Cria janela com 4 quadros para agrupar os gráficos
s.traject(pro3$scorX, clab = 0)
s.label(pro3$scorX, clab = 0.8, add.p = TRUE)
s.traject(pro3$scorY, clab = 0)
s.label(pro3$scorY, clab = 0.8, add.p = TRUE)
s.arrow(pro3$loadX, clab = 0.75)
s.arrow(pro3$loadY, clab = 0.75)
layout(1)

plot(pro3) # Plota os resultados anteriores de trajetória das duas ordenações, e a projeção conjunta com o ajuste da análise de Procrustes

protest1 <- procuste.randtest(pca1$tab, pca2$tab, 999) # Teste de significância

plot(protest1, main = "PROTEST", ylab = "Frequência", xlab = "Valor observado") # Plota o valor gerado pela estatística de Monte-Carlo
