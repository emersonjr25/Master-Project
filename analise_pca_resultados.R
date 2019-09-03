#PCA Analysis - option 1
install.packages("stats")
install.packages("ggplot2")
library(stats)
library(ggplot2)
dados <- read.csv()
view(data)
pcaresults <- prcomp (dados, scale = TRUE)
pcaresults
screenplot(pcaresults)
plot(pcaresults)
summary(pcaresults)
biplot(pcaresults)
ggplot(data = pcaresults, aes())

#library(FactorMineR)
#PCA(dados)

#PCA Analysis - option 2
library(xlsx)
library(FactorMineR)
library(factoextra)

pcaresults2 <- na.omit(dados)
pcasubresults2 <- pcaresults2(1:12, 3:15)
view(pcasubresults2)
pcaresults2 <- scale(pcasubresults2)
finalresultspca2 <- PCA(pcaresults2, graph = F)
eig.val <- get_eigenvalue(finalresultspca2)
eig.val
fviz_eig(finalresultspca2, addlabels = T, ylin = c(0,90))
var <- get_pca_var(finalresultspca2)
ind <- get_pca_ind(finalresultspca2)
fviz_pca_var(finalresultspca2, col.var = "blue")
grupo <- as.factor(dados[,1])
fviz_pca_biplot(finalresultspca2, habillage = grupo, title = "Gráfico PCA")

