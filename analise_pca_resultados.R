#Packages - install and carry
#install.packages("stats")
#install.packages("ggplot2")
#install.packages("xlsx")
#install.packages("FactorMineR")
#install.packages("factoextra")
library(stats)
library(ggplot2)
library(xlsx)
library(FactoMineR)
library(factoextra)

#PCA Analysis - option 1
setwd("C:/Users/emers/Dropbox/Codigos/MestradoEmerson-master/MestradoEmerson-master/MestradoEmerson/Resultado_dados/")
getwd()
dados <- read.csv("C:/Users/emers/Dropbox/Codigos/MestradoEmerson-master/MestradoEmerson-master/MestradoEmerson/Resultado_dados/Variando plasticidade 8 combinacoes/8GeneralPlasticityHighCostHighPerturbationHighfractality.csv", header = TRUE, sep = ";", quote = "\"", dec = ",")
#View(dados)
pcaresults <- prcomp (dados[,12:24], scale = TRUE)
pcaresults
screeplot(pcaresults)
plot(pcaresults)
summary(pcaresults)
biplot(pcaresults)

#PCA Analysis - option 2
pcaresults2 <- na.omit(dados)
pcaresultsomit2 <- pcaresults2[, 12:24]
#View(pcaresultsomit2)
pcaresultsomit2 <- scale(pcaresultsomit2)
#View(pcaresultsomit2)
finalresultspca2 <- PCA(pcaresultsomit2, graph = F)
eig.val <- get_eigenvalue(finalresultspca2)
eig.val
fviz_eig(finalresultspca2, addlabels = T, ylin = c(0,90))
var <- get_pca_var(finalresultspca2)
ind <- get_pca_ind(finalresultspca2)
fviz_pca_var(finalresultspca2, col.var = "blue")
grupo <- as.factor(dados[,2])
fviz_pca_biplot(finalresultspca2, habillage = grupo, title = "Gráfico PCA")


