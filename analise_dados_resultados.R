#Packages - install and carry
#install.packages("stats")
#install.packages("ggplot2")
#install.packages("xlsx")
#install.packages("FactorMineR")
#install.packages
#install.packages("dplyr")
library(stats)
library(ggplot2)
library(xlsx)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(lmtest)
library(car)

#Loading data
setwd("C:/Users/emers/Dropbox/Codigos/MestradoEmerson-master/MestradoEmerson-master/MestradoEmerson/Resultado_dados/")
getwd()
dados <- read.csv("C:/Users/emers/Dropbox/Codigos/MestradoEmerson-master/MestradoEmerson-master/MestradoEmerson/Resultado_dados/Resultados 24 combinacoes completo/test 2.csv", header = TRUE, sep = ";", quote = "\"", dec = ",")
#View(dados)
dados <- na.omit(dados)


#PCA Analysis - option 1
pcaresults <- prcomp (dados[,26:35], scale = TRUE)
pcaresults
screeplot(pcaresults)
plot(pcaresults)
summary(pcaresults)
#biplot(pcaresults)

#PCA Analysis - option 2
pcaresults2 <- na.omit(dados)
pcaresultsomit2 <- pcaresults2[, 26:35]
#View(pcaresultsomit2)
pcaresultsomit2 <- scale(pcaresultsomit2)
#View(pcaresultsomit2)
finalresultspca2 <- PCA(pcaresultsomit2, graph = F)
eig.val <- get_eigenvalue(finalresultspca2)
eig.val
fviz_eig(finalresultspca2)
var <- get_pca_var(finalresultspca2)
ind <- get_pca_ind(finalresultspca2)
fviz_pca_var(finalresultspca2, col.var = "blue")
grupo <- as.factor(pcaresults2[, 1])
fviz_pca_biplot(finalresultspca2, habillage = grupo, geom = "point", title = "Gráfico PCA")

#plot - Pré e pós perturbação de cada uma das 24 combinação de parametros nas variaveis Shannon e equabilidade
plot(dados$Shannon, dados$Evenness, col = dados$ticks)

#Normality
#shapiro.test(dados$perturbation)

#Homogeinity
leveneTest(dados$Shannon ~ dados$plasticity)

#MANOVA
ANOVA1 <- manova(cbind(dados$Shannon, dados$Evenness) ~ dados$plasticity * dados$cost * dados$perturbation, data = dados)
summary(ANOVA1)
plot(cbind(dados$Shannon, dados$Evenness) ~ dados$plasticity * dados$cost * dados$perturbation)

