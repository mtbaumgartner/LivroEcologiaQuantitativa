rm(list = ls())

# Procedimentos para calcular a RDA no programa R

require(vegan) # Carregar o pacote 'vegan'
data(varespec) # Conjunto de dados (matriz) que consiste na
# estimativa da cobertura de 44 espécies de sub-bosque
data(varechem) # Conjunto de dados (matriz) que consiste nas
# características de solo dos mesmos sítios amostrais do 'varespec'
help(decostand) # Página de ajuda para a função 'decostand'

vare.hel <- decostand(varespec, method = "hellinger") # Padronização
# da matriz resposta pela transformação de Hellinger (definido
# no argumento 'method')

# Alguns dos gráficos de dispersão para avaliar linearidade
# (ausência de relação unimodal):

plot(varechem[,1], vare.hel[,1]) # No primeiro argumento está a
# variável que estará no eixo X (preditora) e o segundo a que
# estará no eixo Y (resposta). '[,1]'= indica a coluna 1 do
# objeto à esquerda do '['; '[,2]' = coluna 2, etc.
plot(varechem[,2], vare.hel[,35])
plot(varechem[,12], vare.hel[,22])
plot(varechem[,11], vare.hel[,28])

# RDA básica (inclui todas as variáveis preditoras) e seleção
# de variáveis (seleção do melhor modelo de RDA):
help(rda) # Página de ajuda para a função 'rda'

rda.ex0 <- rda(vare.hel ~ 1, varechem) # Modelo de RDA somente com o
# intercepto, isto é, modelo sem nenhuma variável preditora.

rda.ex1 <- rda(vare.hel ~ ., varechem) # Modelo de RDA com
# todas as variáveis preditoras (RDA básica)

help(ordistep) # Página de ajuda para a função 'ordistep'
selecao.modelos <- ordistep(rda.ex0, scope = formula(rda.ex1),
                            direction = "both", pstep = 1000)
# Seleção de variáveis para serem #incluídas no modelo final da RDA
selecao.modelos # Mostra o modelo final escolhido

rda.final <- rda(vare.hel ~ varechem$Al + varechem$K) # A seleção de
# modelos indicou somente a inclusão das variáveis 'Al' e 'K'

vif.cca(rda.final) # teste de multicolinearidade
permutest(rda.final, permutations = 999) # função para testar a
# significância global da analise
anova.cca(rda.final, by = "axis") # função para testar a
# significância dos eixos
RsquareAdj(rda.final) # função que retorna o R² e R² ajustado
plot(rda.final)
scores(rda.final) # extrair os escores das espécies e amostras 
