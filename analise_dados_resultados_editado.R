#Packages - install and carry
#install.packages("stats")
#install.packages("ggplot2")
#install.packages("agricolae")
#install.packages("FactorMineR")
#install.packages("dplyr")
#install.packages("vegan")
#install.packages("agricolae")
#install.packages("ggpubr")
#install.packages("hexbin")
#install.packages("hrbrthemes")
#install.packages("viridis")
#install.packages("olsrr")
#install.packages("MASS")
library(olsrr)
library(MASS)
library(ggpubr)
library(stats)
library(ggplot2)
library(dplyr)
library(lmtest)
library(car)
library(here)
library(vegan)
library(agricolae)
library(hexbin)
library(hrbrthemes)
library(viridis)

#Loading data
dados <- read.csv(here("Resultado_dados/Dados Brutos/dados_brutos_grande_mundo_shannon_dist.csv"), header = TRUE, sep = ";", quote = "\"", dec = ",")

#Transformacao geral de fatores numericos em categoricos / organizando
str(dados)
dados$fractality = NA
dados$perturbation = NA
levels(dados$model.version)

dados$fractality[dados$model.version == "HighPerturbationHighfractality"] = "high"
dados$perturbation[dados$model.version == "HighPerturbationHighfractality"] = "high"

dados$fractality[dados$model.version == "HighPerturbationLowfractality"] = "low"
dados$perturbation[dados$model.version == "HighPerturbationLowfractality"] = "high"

dados$fractality[dados$model.version == "LowPerturbationHighfractality"] = "high"
dados$perturbation[dados$model.version == "LowPerturbationHighfractality"] = "low"

dados$fractality[dados$model.version == "LowPerturbationLowfractality"] = "low"
dados$perturbation[dados$model.version == "LowPerturbationLowfractality"] = "low"

dados$cost_plasticity[dados$cost_plasticity_sheep == 0] = "NA"
dados$cost_plasticity[dados$cost_plasticity_sheep == 0.2] ="low"
dados$cost_plasticity[dados$cost_plasticity_sheep == 0.8] ="high"

dados$level_plasticity[dados$sheep_plasticity == 0] ="Without"
dados$level_plasticity[dados$sheep_plasticity == 2] ="With"
dados$level_plasticity[dados$sheep_plasticity == 5] ="With"
dados$level_plasticity[dados$sheep_plasticity == 8] ="With"


#Dataframe geral - tabela com variaveis preditoras para os 3 objetivos

colnames(dados)
shannon.dists = data.frame("sheep_plasticity"= rep(NA, length(dados$ticks)),
                         "wolf_plasticity"= rep(NA, length(dados$ticks)),
                         "cost_plasticity_sheep"= rep(NA, length(dados$ticks)), 
                         "cost_plasticity_wolf"= rep(NA, length(dados$ticks)), 
                         "sheep_gain_from_food"= rep(NA, length(dados$ticks)), 
                         "wolf_gain_from_food"=rep(NA, length(dados$ticks)), 
                         "sheep_reproduce" = rep(NA, length(dados$ticks)),  
                         "wolf_reproduce" = rep(NA, length(dados$ticks)), 
                         "grass_regrowth_time"= rep(NA, length(dados$ticks)), 
                         "model.version" = rep(NA, length(dados$ticks)), 
                         "replicate.number" = rep(NA, length(dados$ticks)), 
                         "fractality" = rep(NA, length(dados$ticks)), 
                         "perturbation"= rep(NA, length(dados$ticks)),
                         "cost_plasticity"= rep(NA, length(dados$ticks)),
                         "level_plasticity"= rep(NA, length(dados$ticks)))

#plasticity effect resilience (general shannon dist)

i=1
for( i in 1:length(dados$ticks)){
if(i %% 2!=0){ 
shannon.dists$Shannon.dists[i]        = abs(dados$Shannon[i] - dados$Shannon[i + 1])
shannon.dists$eveness.dists[i]        = abs(dados$Evenness[i] - dados$Evenness [ i + 1])
shannon.dists$richness.dists[i]       = abs(dados$Richness[i] - dados$Richness [ i + 1])
shannon.dists$shannonprey.dists[i]    = abs(dados$shannonprey[i] - dados$shannonprey[i + 1])
shannon.dists$shannonpredator.dists[i] = abs(dados$shannonpredator[i] - dados$shannonpredator[i + 1])
shannon.dists$shannonspecialist.dists[i] = abs(dados$shannonspecialist[i] - dados$shannonspecialist[i + 1])
shannon.dists$shannonspecialistandgeneralist.dists[i] = abs(dados$shannonspecialistandgeneralist[i] - dados$shannonspecialistandgeneralist[i + 1])
shannon.dists$shannongeneralist[i] = abs(dados$shannongeneralist[i] - dados$shannongeneralist[i + 1])
shannon.dists$sheep_plasticity[i]     = dados$sheep_plasticity[i]
shannon.dists$wolf_plasticity[i]      = dados$wolf_plasticity[i]
shannon.dists$cost_plasticity_sheep[i]= dados$cost_plasticity_sheep[i]
shannon.dists$cost_plasticity_wolf[i] = dados$cost_plasticity_wolf[i]
shannon.dists$sheep_gain_from_food[i] = dados$sheep_gain_from_food[i]
shannon.dists$wolf_gain_from_food[i]  = dados$wolf_gain_from_food[i]
shannon.dists$sheep_reproduce[i]      = dados$sheep_reproduce[i]
shannon.dists$wolf_reproduce[i]       = dados$wolf_reproduce[i]
shannon.dists$grass_regrowth_time[i]  = dados$grass_regrowth_time[i]
shannon.dists$model.version[i]        = dados$model.version[i]
shannon.dists$replicate.number[i]     = dados$replicate.number[i]
shannon.dists$fractality[i]           = dados$fractality[i]
shannon.dists$perturbation[i]         = dados$perturbation[i]
shannon.dists$cost_plasticity[i]      = dados$cost_plasticity[i]     
shannon.dists$level_plasticity[i]     = dados$level_plasticity[i]     
}
}
shannon.dists = na.omit(shannon.dists)

shannon.dists$sheep_plasticity      = as.factor(shannon.dists$sheep_plasticity      )
shannon.dists$wolf_plasticity       = as.factor(shannon.dists$wolf_plasticity       )
shannon.dists$cost_plasticity_sheep = as.factor(shannon.dists$cost_plasticity_sheep )
shannon.dists$cost_plasticity_wolf  = as.factor(shannon.dists$cost_plasticity_wolf  )
shannon.dists$sheep_gain_from_food  = as.factor(shannon.dists$sheep_gain_from_food  )
shannon.dists$wolf_gain_from_food   = as.factor(shannon.dists$wolf_gain_from_food   )
shannon.dists$sheep_reproduce       = as.factor(shannon.dists$sheep_reproduce       )
shannon.dists$wolf_reproduce        = as.factor(shannon.dists$wolf_reproduce        )
shannon.dists$grass_regrowth_time   = as.factor(shannon.dists$grass_regrowth_time   )
shannon.dists$model.version         = as.factor(shannon.dists$model.version         )
shannon.dists$replicate.number      = as.factor(shannon.dists$replicate.number      )
shannon.dists$fractality            = as.factor(shannon.dists$fractality            )
shannon.dists$perturbation          = as.factor(shannon.dists$perturbation          )
shannon.dists$cost_plasticity       = as.factor(shannon.dists$cost_plasticity       )
shannon.dists$level_plasticity      = as.factor(shannon.dists$level_plasticity      )

shannon.dists=droplevels(shannon.dists)
shannon.dists <- na.omit(shannon.dists)
str(shannon.dists)
#View(shannon.dists)


#rearrange factor levels
shannon.dists$level_plasticity <- factor(shannon.dists$level_plasticity, levels = c("Without", "With")) #Without and With # Low Medium High
#shannon.dists$cost_plasticity <- factor(shannon.dists$cost_plasticity, levels = c("low", "high"))
shannon.dists$perturbation <- factor(shannon.dists$perturbation, levels = c("low", "high"))
shannon.dists$fractality <- factor(shannon.dists$fractality, levels = c("low", "high"))
shannon.dists$Shannon.dists = shannon.dists$Shannon.dists * -1


#Model Selection
modelgeneral2 <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity + shannon.dists$fractality + shannon.dists$perturbation, na.action = na.omit)
#ols_step_all_possible(modelgeneral2)
#plot (modelgeneral2)
#res <- resid(modelgeneral2)
#par(mfrow=c(2,2))

###AIC - forward
one <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity)
#two <- lm(shannon.dists$Shannon.dists ~ shannon.dists$fractality)
#three <- lm(shannon.dists$Shannon.dists ~ shannon.dists$perturbation)
#four <- lm(shannon.dists$Shannon.dists ~ shannon.dists$cost_plasticity)
five <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$perturbation)
six <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity)
seven <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$fractality)
eight <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$perturbation + shannon.dists$cost_plasticity)
nine <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$perturbation + shannon.dists$fractality)
ten <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity + shannon.dists$fractality)
eleven <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$perturbation + shannon.dists$cost_plasticity + shannon.dists$fractality)
#twelve <- lm(shannon.dists$Shannon.dists ~ shannon.dists$fractality + shannon.dists$perturbation)
#thirteen <- lm(shannon.dists$Shannon.dists ~ shannon.dists$cost_plasticity + shannon.dists$fractality)
#fourteen <- lm(shannon.dists$Shannon.dists ~ shannon.dists$cost_plasticity + shannon.dists$fractality + shannon.dists$perturbation)
#fifteen <- lm(shannon.dists$Shannon.dists ~ shannon.dists$cost_plasticity + shannon.dists$perturbation)

#Verify multicolinearity - Variance inflation factors (VIF)
#ols_vif_tol(one)
ols_vif_tol(five)
ols_vif_tol(six)
ols_vif_tol(seven)
ols_vif_tol(eight)
ols_vif_tol(nine)
ols_vif_tol(ten)
ols_vif_tol(eleven)

AIC(one)
#AIC(two)
#AIC(three)
#AIC(four)
AIC(five)
AIC(six)
AIC(seven)
AIC(eight)
AIC(nine)
AIC(ten)
AIC(eleven)
#AIC(twelve)
#AIC(thirteen)
#AIC(fourteen)
#AIC(fifteen)

resone <- resid(one)
#restwo <- resid(two)
#resthree <- resid(three)
#resfour <- resid(four)
resfive <- resid(five)
ressix <- resid(six)
resseven <- resid(seven)
reseight <- resid(eight)
resnine <- resid(nine)
resten <- resid(ten)
reseleven <- resid(eleven)
#restwelve <- resid(twelve)
#resthirteen <- resid(thirteen)
#resfourteen <- resid(fourteen)
#resfifteen <- resid(fifteen)

plot(resone)
#plot(restwo)
#plot(resthree)
#plot(resfour)
plot(resfive)
plot(ressix)
plot(resseven)
plot(reseight)
plot(resnine)
plot(resten)
plot(reseleven)
#plot(restwelve)
#plot(resthirteen)
#plot(resfourteen)
#plot(fifteen)