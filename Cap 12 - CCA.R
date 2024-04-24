rm(list = ls())

# Procedimentos para calcular a CCA no programa R

library(vegan) # pacote vegan de funções no software R
?cca # explicação da análise CCA
data(varechem) # entrada da matriz preditora, dados ambientais
data(varespec) # entrada da matriz resposta, dados de abundância de espécies
ordistep(cca(varespec ~ ., varechem)) # seleciona as variáveis
# ambientais que mais explicam a variação da matriz resposta, 
# utilizando uma seleção do tipo stepwise com critério de
# informação de Akaike (AIC). A função ordistep faz parte do
# pacote vegan

env_sel <- varechem[,c(2, 3, 6, 11, 12, 13)] # criando um objeto
# contendo apenas as variáveis selecionadas no melhor modelo

cor(env_sel) # verifica multicolinearidade na matriz preditora
resu.cca <- cca(varespec ~ ., varechem) #cria o objeto resu.cca,
# que salva as informações da análise

vif.cca(resu.cca) # verifica multicolinearidade da matriz
# preditora. Valores acima de 10 indicam variáveis redundantes.
# A função 'vif.cca' deve ser rodada com o resultado da CCA e
# não com a variável preditora como na função 'cor'

summary(resu.cca) # sumariza os resultados
anova.cca(resu.cca) # verifica a significância global da análise
anova.cca(resu.cca, by = "axis") # verifica a significância por eixo da análise 

### Gráficos CCA

?plot.cca
plot(resu.cca) # plota o triplot da CCA
plot(resu.cca, display = c("wa")) # plota apenas os escores dos locais
plot(resu.cca, display = c("wa", "cn")) # plota escores dos locais e centroide das variáveis ambientais
plot(resu.cca, display = c("sp")) # plota apenas os escores das espécies
plot(resu.cca, display = c("sp", "cn")) # plota escores das espécies e centroide das variáveis ambientais

## No argumento 'display=c("")', é possível incluir as seguintes
# alternativas: "species" ou "sp" para escores das espécies,
# "sites" ou "wa" para escores dos locais, "lc" ou "LC scores"
# para restrições lineares, "bp" para setas do escores do biplot,
# ou "cn" para os centroides das restrições dos fatores em vez de
# uma seta. A escolha dependerá do que o usuário quer ver no 'plot'.

