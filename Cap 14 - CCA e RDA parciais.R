rm(list = ls())

# Procedimentos para calcular a RDA e a CCA parcial no programa R

### Carregar os pacotes
library(vegan)
library(adespatial) # incorporou as análises do pacote 'packfor'

### Importar os dados biológicos do banco de dados do R
data(mite)

### Importar os dados ambientais do banco de dados do R
data(mite.env)
attach(mite.env)

### Importar os dados espaciais do banco de dados do R
data(mite.pcnm)
attach(mite.pcnm)

###RDA parcial

## Transformar os dados biológicos 
bio.hel <- decostand(mite, "hellinger") # transformação de Hellinger
bio.hel

# ambientais = dependendo dos dados ambientais, estas devem ser transformados.

## Seleção forward de modelos

# variáveis ambientais
rda.envi <- rda(bio.hel ~ 1, data = mite.env) # modelo apenas com intercepto
rda.env <- rda(bio.hel ~ ., data = mite.env) # modelo com todas as variáveis ambientais
seleçao.env <- ordistep(rda.envi, data = mite.env, scope = formula(rda.env),
                        direction = "forward", pstep = 999)
rda.env.sel <- rda(bio.hel ~ WatrCont + Topo + SubsDens + Shrub + Substrate)
vif.cca(rda.env.sel) # colinearidade

# variáveis espaciais
rda.spai <- rda(bio.hel ~ 1, data = mite.pcnm) # modelo com intercepto
rda.spa <- rda(bio.hel ~ ., data = mite.pcnm) # modelo com todas as variáveis espaciais
seleçao.spa <- ordistep(rda.spai, data = mite.pcnm, scope = formula(rda.spa),
                        direction = "forward", perm.max = 200)
rda.spa.sel <- rda(bio.hel ~ V2 + V3 + V8 + V1 + V6 + V4 + V9 + V16 + V7 + V20 + V11 + V10)
vif.cca(rda.spa.sel) # colinearidade

## Seleção de modelos pelo critério proposto por Blanchet, Legendre e Borcard (2008)
rda.env <- rda(bio.hel ~ ., data = mite.env)
R2.env <- RsquareAdj(rda.env)$r.squared
seleçao.envB <- forward.sel(bio.hel, mite.env[,1:2], adjR2thresh = R2.env) # a matriz de dados ambientais deve ser numérica
seleçao.envB

## Cálculo da pRDA
env <- cbind(WatrCont, Topo, SubsDens, Shrub, Substrate)
env
spa <- cbind(V2, V3, V8, V1, V6, V4, V9, V16, V7, V20, V11, V10)
spa

rda.parcial <- varpart(bio.hel, env,spa)
rda.parcial
plot(rda.parcial)

## Testar a significância das frações

# fração [a]
rda.env.spa <- rda(bio.hel, env, spa) 
sig.a <- anova(rda.env.spa, pstep = 999) 
sig.a

#fração [c]
rda.spa.env <- rda(bio.hel, spa,env)
sig.c <- anova(rda.spa.env, pstep = 999)
sig.c


### CCA parcial

## Seleção de modelos - variáveis ambientais
cca.envi <- cca(mite ~ 1, data = mite.env) # modelo com intercepto
cca.env <- cca(mite ~ ., data = mite.env) # modelo com todas as variáveis ambientais
seleçao.env <- ordistep(cca.envi, data = mite.env, scope = formula(cca.env),
                        direction = "forward", pstep = 999)
cca.env.sel <- cca(mite ~ WatrCont + SubsDens + Topo + Shrub + Substrate)
vif.cca(cca.env.sel) # colinearidade

## Seleção de modelos - variáveis espaciais
cca.spai <- cca(mite ~ 1, data = mite.pcnm) # modelo com intercepto
cca.spa <- cca(mite ~ ., data = mite.pcnm) # modelo com todas as variáveis espaciais
seleçao.spa <- ordistep(cca.envi, data = mite.pcnm, scope = formula(cca.spa),
                        direction = "forward", pstep = 999)
cca.spa.sel <- cca(mite ~ V2 + V6 + V3 + V10 + V8 + V4 + V16 + V5 + V1 + V20 + V9)
vif.cca(cca.spa.sel) # colinearidade

## CCA ambiental e espacial
env <- cbind(WatrCont, SubsDens, Topo, Shrub, Substrate)
env
spa <- cbind(V2, V6, V3, V10, V8, V4, V16, V5, V1, V20, V9)
spa

env.mat <- as.matrix(env)
env.mat
spa.mat <- as.matrix(spa)
spa.mat

cca.total <- cca(mite ~ env.mat + spa.mat)
cca.total
anova(cca.total, pstep = 999)

## CCA ambiental
cca.env <- cca(mite ~ env.mat)
cca.env
anova(cca.env, pstep = 999)

## CCA espacial
cca.spa <- cca(mite ~ spa.mat)
cca.spa
anova(cca.spa, pstep = 999)

## Testar a significância dos componentes

## fração [a]
cca.env.spa <- cca(mite, env,spa)
sig.a <- anova(cca.env.spa, pstep = 999)
sig.a

##fração [c]
cca.spa.env <- rda(bio.hel, spa, env)
sig.c <- anova(cca.spa.env, pstep = 999)
sig.c
