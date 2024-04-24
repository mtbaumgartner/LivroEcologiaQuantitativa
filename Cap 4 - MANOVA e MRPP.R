rm(list = ls())


# Procedimentos para calcular a MANOVA no programa R

## Exemplo sobre a produção de filme plástico (Johnson; Wichern, 1992):

resistencia <- c(6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3, 6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6) # Cria um vetor com 20 observações

brilho <- c(9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4, 9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2) # Cria um vetor com 20 observações

opacidade <- c(4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7, 2.8, 4.1, 3.8, 1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9) # Cria um vetor com 20 observações

Y <- cbind(resistencia, brilho, opacidade)
# 'cbind' junta os três vetores em um novo objeto, i.e., uma matriz com três variáveis

taxa <- factor(gl(2, 10), labels = c("Baixo", "Alto")) # Cria um fator com dois níveis: "Baixo" e "Alto"

aditivo <- factor(gl(2, 5, length = 20), labels = c("Baixo", "Alto")) # Cria um fator com dois níveis: "Baixo" e "Alto"

# Teste de normalidade pela distância de Mahalanobis:
qqnorm(mahalanobis(Y, colMeans(Y), cov(Y)))
# A função 'qqnorm' produz um gráfico de dispersão com as distâncias observadas no eixo Y e as distâncias de uma variável teórica com distribuição normal no eixo X
# 'mahanalobis' função que computa a distância de Mahalanobis; o argumento 'colMeans' corresponde às médias das colunas da matriz "Y", 'cov' covariância da matriz "Y"

qqline(mahalanobis(Y, colMeans(Y), cov(Y)))
# A função 'qqline' produz uma reta representando a distribuição normal teórica

library('MVN')
mvn(Y) # Teste de Henze-Zirkler sobre a matriz resposta

# Teste de esfericidade:
mauchly.test(lm(Y ~ 1)) # As covariâncias não são homogêneas

# Modelo da MANOVA
manova.pla <- manova(Y ~ taxa * aditivo) # '~' argumento que expressa o modelo de uma função estatística no R, '*' argumento que indica interação entre os fatores

summary(manova.pla, test = "Wilks") # O argumento 'test' define qual estatística teste será retornada (nesse caso será retornado o Lambda de Wilks)

summary(manova.pla, test = "Pillai") # Traço de Pillai

summary(manova.pla, test = "Hotelling-Lawley") # Traço de Hotelling-Lawley

summary(manova.pla, test = "Roy") # Maior raiz de Roy

# Correção nos graus de liberdade devido à ausência de esfericidade

man.pla_tab <- summary(manova.pla)
with(man.pla_tab, 1 - pf(q = stats[, 3], df1 = stats[, 4] * (1 / (ncol(Y) - 1)), df2 = stats[, 5] * (1 / (ncol(Y) - 1))))
# Correção nos graus de liberdade pelo menor valor do fator de correção epsilon.

# Testes post-hoc (ANOVA unifatorial com correção de Bonferroni; não utilizamos o teste de Tukey, pois, cada fator apresenta somente dois níveis). Como realizaremos 3 testes o nível de significância corrigido é 0.05/3 = 0.017

aov.resistencia.taxa <- aov(Y[, 1] ~ taxa)
# A função 'aov' indica a realização de Análise de Variância; o argumento '[,]' indica a seleção de linhas (a esquerda da ',') e/ou colunas (a direita da ',') de um objeto, nesse caso a coluna 1

aov.brilho.taxa <- aov(Y[, 2] ~ taxa)
aov.opacidade.taxa <- aov(Y[, 3] ~ taxa)

summary(aov.resistencia.taxa)
summary(aov.brilho.taxa)
summary(aov.opacidade.taxa)

plot(Y[, 1] ~ taxa)
# Gráfico para avaliar qual dos níveis de 'taxa' apresentaram os maiores valores de 'resistencia'.
# Pelos testes a posteriori podemos observar que as diferenças detectadas pela MANOVA foram devidas à variável 'resistencia'.
# Além disso, em conjunto as variáveis diferem entre os níveis do fator 'aditivo', entretanto, individualmente os níveis são similares.


# Procedimentos para calcular a MRPP no programa R

require(vegan) # Carregar pacote

data(mite) # Ler conjunto de dados que está no pacote vegan
# 'mite' consiste em dados de abundâncias de espécies de ácaros coletados por Borcard & Legendre (1994)

data(mite.env) # 'mite.env' consiste em dados ambientais (densidade de substrato, conteúdo de água, tipo de substrato, densidade de arbusto e microtopografia)

acaro <- mite # Nomear esse conjunto de dados para 'acaro'

acaro.amb <- mite.env # Nomear esse conjunto de dados para 'acaro.amb'

acaro.tot <- cbind(acaro.amb$Shrub, acaro)

mrpp.acaro <- mrpp(acaro, grouping = acaro.amb$Shrub, permutations = 999, distance = "bray", weight.type = 1) # A função 'mrpp' realiza o cálculo da MRPP, o primeiro argumento consiste na matriz resposta, o segundo ('grouping') consiste na variável preditora.
# Por padrão essa função assume a distância euclidiana, 999 permutações e, como peso, o número de observações por amostra. Alteramos somente a distância para Bray-Curtis por ser mais recomendada para dados de abundância.
# A métrica de distância pode ser alterada no argumento 'distance'; o número de permutações no argumento 'permutations'; e o peso no argumento 'weight.type', este assume os valores 1, 2 e 3 que utilizam como peso o n, n-1 e n*(n-1), respectivamente (n = tamanho amostral).

mrpp.acaro

# Post-hoc (MRPP par a par, com correção de Bonferroni):
# Nível de significância corrigido é 0.05/3 = 0.017

few.many <- subset(acaro.tot, acaro.tot[, 1] %in% c("Few", "Many"))
few.none <- subset(acaro.tot, acaro.tot[, 1] %in% c("Few", "None"))
many.none <- subset(acaro.tot, acaro.tot[, 1] %in% c("Many", "None"))
mrpp.f.m <- mrpp(few.many[, 2:36], few.many[, 1], distance = "bray")
mrpp.f.n <- mrpp(few.none[, 2:36], few.none[, 1], distance = "bray")
mrpp.m.n <- mrpp(many.none[, 2:36], many.none[, 1], distance = "bray")
mrpp.f.m
mrpp.f.n
mrpp.m.n
# Com esses resultados podemos observar que todos os níveis diferem significativamente
