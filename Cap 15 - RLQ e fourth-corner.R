rm(list = ls())

# Procedimentos para calcular RLQ, RLQ parcial e Fourth-corner no programa R

### RLQ

library(ade4) # carregar o pacote 'ade4'
data(aravo) # banco de dados de onde as matrizes serão extraídas
matrizL.aravo <- dudi.coa(aravo$spe, scannf = FALSE) # Ordenação da matriz L
matrizR.aravo <- dudi.hillsmith(aravo$env, row.w = matrizL.aravo$lw, scannf = FALSE) # Ordenação da matriz R
matrizQ.aravo <- dudi.pca(aravo$traits, row.w = matrizL.aravo$cw, scannf = FALSE) #Ordenação da matriz Q
rlq.aravo <- rlq(matrizR.aravo, matrizL.aravo, matrizQ.aravo, scannf = FALSE) # Função rlq() utilizando as três matrizes acima
summary(rlq.aravo) # resumo dos resultados
plot(rlq.aravo) # visualizar o gráfico da rlq

# Plotar o gráfico
par(mfrow = c(1, 3))
s.arrow(rlq.aravo$l1)
s.arrow(rlq.aravo$c1)
s.label(rlq.aravo$lQ, boxes = FALSE)
100 * rlq.aravo$eig / sum(rlq.aravo$eig) # % de co-inércia para cada eixo
t(matrizR.aravo$tab) %*% (diag(matrizR.aravo$lw)) %*% as.matrix(rlq.aravo$mR) # correlações para cada uma das variáveis ambientais
t(matrizQ.aravo$tab) %*% (diag(matrizQ.aravo$lw)) %*% as.matrix(rlq.aravo$mQ) # valores de correlações para os traços funcionais
rlq.aravo$tab # correlações entre variáveis ambientais e traços funcionais

### RLQ parcial

library(ade4) # carregar o pacote
data(piosphere) # carregar o banco de dados
matrizL <- dudi.coa(log(piosphere$veg + 1), scannf = FALSE) # ordenação dos dados da matriz L
matrizR <- dudi.pca(piosphere$env, scannf = FALSE, row.w = matrizL$lw) # ordenação dos dados da matriz R
matrizQ <- dudi.hillsmith(piosphere$traits, scannf = FALSE, row.w = matrizL$cw) # ordenação dos dados da matriz Q
rlq1 <- rlq(matrizR, matrizL, matrizQ, scannf = FALSE) # primeiramente, rodar uma rlq igual ao script anterior
wrlq <- wca(rlq1, fac = piosphere$habitat, scannf = FALSE) # função para rlq parcial utilizando a covariável 'habitat'
summary(wrlq) # visualizar um resumo dos resultados
plot(wrlq) # visualizar gráfico

par(mfrow = c(1, 3)) # plotar gráficos separadamente
s.arrow(wrlq$l1) # variaveis ambientais
s.arrow(wrlq$c1) # traços funcionais
s.label(wrlq$lQ) # espécies
100 * wrlq$eig / sum(rlq1$eig) # Porcentagem de co-inercia para cada eixo
t(matrizR$tab) %*% (diag(matrizR$lw)) %*% as.matrix(wrlq$mR) # extrair valores de covariância/correlação para cada uma das variáveis ambientais
t(matrizQ$tab) %*% (diag(matrizQ$lw)) %*% as.matrix(wrlq$mQ) # extrair valores de covariância/correlação para os traços funcionais
wrlq$tab # extrair valores de correlação entre as variáveis ambientais e os traços funcionais

### Fourth-corner

library(ade4) # carregar pacote
data(aravo) # carregar banco de dados
env <- aravo$env # criar matriz com os dados ambientais
spe <- aravo$spe # criar matriz com os dados das espécies
traços <- aravo$traits # criar matriz com os traços
1 / (0.05 / (ncol(env) * ncol(traços))) # estabelecer número de permutações mínimas considerando o conjunto de dados e o valor de α
fourth <- fourthcorner(env, spe, traços, nrepet = 999, modeltype = 6, p.adjust.method.G = p.adjust.methods) # função para realizar a
# fourthcorner, o tipo de modelo deve ser pensado previamente
summary(fourth) # resumo dos resultados
plot(fourth, stat = 'G') # visualizar o gráfico com os resultados
