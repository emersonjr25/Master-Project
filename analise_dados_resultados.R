#Packages - install and carry
#install.packages("stats")
#install.packages("ggplot2")
#install.packages("agricolae")
#install.packages("dplyr")
#install.packages("vegan")
#install.packages("agricolae")
#install.packages("ggpubr")
library(ggpubr)
library(stats)
library(ggplot2)
library(dplyr)
library(lmtest)
library(car)
library(here)
library(vegan)
library(agricolae)

#Loading data
dados <- read.csv(here("Resultado_dados/Dados brutos/dados_brutos_total_com_e_sem_plasticidade.csv"), header = TRUE, sep = ";", quote = "\"", dec = ",")

#plot - Pre e pos perturbacao de cada uma das 24 combinacao de parametros nas variaveis Shannon e equabilidade
#plot(dados$Shannon[2:1001], dados$Shannon[1002:2001], col = dados$ticks)
#plot(dados$Shannon[2:1001], dados$Shannon[1002:2001], col = dados$ticks)
#plot(dados$replicate.number, dados$Shannon, col = dados$ticks)


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

dados$cost_plasticity[dados$cost_plasticity_sheep == 0] = "no"
dados$cost_plasticity[dados$cost_plasticity_sheep == 0.2] ="low"
dados$cost_plasticity[dados$cost_plasticity_sheep == 0.8] ="high"

dados$level_plasticity[dados$sheep_plasticity == 0] ="no"
dados$level_plasticity[dados$sheep_plasticity == 2] ="low"
dados$level_plasticity[dados$sheep_plasticity == 5] ="medium"
dados$level_plasticity[dados$sheep_plasticity == 8] ="high"



#calculate shannon trophical level and specialization
dados$shannonprey = diversity(dados[30:32], index = "shannon")
dados$shannonpredator = diversity(dados[33:35], index = "shannon")

specialist <- data.frame("specialistone" = c(dados[30]),
                         "specialisttwo" = c(dados[33]) )
dados$shannonspecialist =  diversity(specialist, index = "shannon") #sheepone and wolvesone


shannonspecialistandgeneralist <- data.frame("spegenone" = c(dados[31]),
                                             "spegentwo" = c(dados[34]) )
dados$shannonspecialistandgeneralist = diversity(shannonspecialistandgeneralist, index = "shannon") #sheepthree and wolvestwo


shannongeneralist <- data.frame("generalistone" = c(dados[32]),
                                "generalisttwo" = c(dados[35]) )
dados$shannongeneralist = diversity(shannongeneralist, index = "shannon")  #sheepfour and wolvesfour



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
str(shannon.dists)
#View(shannon.dists)

#rearrange factor levels
shannon.dists$level_plasticity <- factor(shannon.dists$level_plasticity, levels = c("no", "low", "medium", "high"))
shannon.dists$cost_plasticity <- factor(shannon.dists$cost_plasticity, levels = c("no", "low", "high"))
shannon.dists$perturbation <- factor(shannon.dists$perturbation, levels = c("low", "high"))
shannon.dists$fractality <- factor(shannon.dists$fractality, levels = c("low", "high"))

#ANOVA general

modelgeneral1 = aov(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity +shannon.dists$fractality + shannon.dists$perturbation )
summary(modelgeneral1)
modelgeneral2 = lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity +shannon.dists$fractality + shannon.dists$perturbation )
summary(modelgeneral2)
plot(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity + shannon.dists$fractality + shannon.dists$perturbation)

#ANOVA prey and predator

modelprey1 = aov(shannon.dists$shannonprey.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity +shannon.dists$fractality + shannon.dists$perturbation )
summary(modelprey1)
modelprey2 = lm(shannon.dists$shannonprey.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity +shannon.dists$fractality + shannon.dists$perturbation )
summary(modelprey2)
plot(shannon.dists$shannonprey.dists  ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity + shannon.dists$fractality + shannon.dists$perturbation)

modelpredator1 = aov(shannon.dists$shannonpredator.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity +shannon.dists$fractality + shannon.dists$perturbation )
summary(modelpredator1)
modelpredator2 = lm(shannon.dists$shannonpredator.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity +shannon.dists$fractality + shannon.dists$perturbation )
summary(modelpredator2)
plot(shannon.dists$shannonpredator.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity + shannon.dists$fractality + shannon.dists$perturbation)

#ANOVA specialist, specialistgeneralist, generalist

modelspecialist1 = aov(shannon.dists$shannonspecialist.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity +shannon.dists$fractality + shannon.dists$perturbation )
summary(modelspecialist1)
modelspecialist2 = lm(shannon.dists$shannonspecialist.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity +shannon.dists$fractality + shannon.dists$perturbation )
summary(modelspecialist2)
plot(shannon.dists$shannonspecialist.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity + shannon.dists$fractality + shannon.dists$perturbation)

modelspecialistandgeneralist1 = aov(shannon.dists$shannonspecialistandgeneralist.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity +shannon.dists$fractality + shannon.dists$perturbation )
summary(modelspecialistandgeneralist1)
modelspecialistandgeneralist2 = lm(shannon.dists$shannonspecialistandgeneralist.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity +shannon.dists$fractality + shannon.dists$perturbation )
summary(modelspecialistandgeneralist2)
plot(shannon.dists$shannonspecialistandgeneralist.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity + shannon.dists$fractality + shannon.dists$perturbation)

modelgeneralist1 = aov(shannon.dists$shannongeneralist ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity +shannon.dists$fractality + shannon.dists$perturbation )
summary(modelgeneralist1)
modelgeneralist2 = lm(shannon.dists$shannongeneralist ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity +shannon.dists$fractality + shannon.dists$perturbation )
summary(modelgeneralist2)
plot(shannon.dists$shannongeneralist ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity + shannon.dists$fractality + shannon.dists$perturbation)


#ggplot general

ggplot(data = shannon.dists, aes(x= level_plasticity , y= Shannon.dists)) +
  ggtitle("Plasticity effects on resilience") +
  geom_boxplot(aes(color = cost_plasticity, fill= perturbation), alpha= 0.3, height = 0.5, width=0.2) +
  scale_fill_manual(values=c("10", "20")) + scale_color_manual(values=c("1", "2", "3")) +
  facet_grid( ~fractality, scales="free_y", space="free") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5)) #+
  # stat_compare_means()
  #annotate("text", x = 2.2, y = 1.20, label = "P value: <2e-16 (all factors), R:0.80", color = "blue", size = 3) #+ rremove("grid") 
  

#ggplot prey and predator

ggplot(data = shannon.dists, aes(x= level_plasticity , y= shannonprey.dists)) +
  ggtitle("Plasticity effects on resilience prey") +
  geom_boxplot(aes(color = cost_plasticity, fill= perturbation), alpha= 0.3, height = 0.5, width=0.2) +
  scale_fill_manual(values=c("10", "20")) + scale_color_manual(values=c("1", "2", "3")) +
  facet_grid( ~fractality, scales="free_y", space="free") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5)) #+
  #stat_compare_means()

ggplot(data = shannon.dists, aes(x= level_plasticity , y= shannonpredator.dists)) +
  ggtitle("Plasticity effects on resilience predator") +
  geom_boxplot(aes(color = cost_plasticity, fill= perturbation), alpha= 0.3, height = 0.5, width=0.2) +
  scale_fill_manual(values=c("10", "20")) + scale_color_manual(values=c("1", "2", "3")) +
  facet_grid( ~fractality, scales="free_y", space="free") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5))# +
  #stat_compare_means()


#ggplot specialist, specialistgeneralist, generalist

ggplot(data = shannon.dists, aes(x= level_plasticity , y= shannonspecialist.dists)) +
  ggtitle("Plasticity effects on resilience specialist") +
  geom_boxplot(aes(color = cost_plasticity, fill= perturbation), alpha= 0.3, height = 0.5, width=0.2) +
  scale_fill_manual(values=c("10", "20")) + scale_color_manual(values=c("1", "2", "3")) +
  facet_grid( ~fractality, scales="free_y", space="free") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5))# +
  #stat_compare_means()

ggplot(data = shannon.dists, aes(x= level_plasticity , y= shannonspecialistandgeneralist.dists)) +
  ggtitle("Plasticity effects on resilience specialist and generalist") +
  geom_boxplot(aes(color = cost_plasticity, fill= perturbation), alpha= 0.3, height = 0.5, width=0.2) +
  scale_fill_manual(values=c("10", "20")) + scale_color_manual(values=c("1", "2", "3")) +
  facet_grid( ~fractality, scales="free_y", space="free") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5)) #+
 # stat_compare_means()

ggplot(data = shannon.dists, aes(x= level_plasticity , y= shannongeneralist)) +
  ggtitle("Plasticity effects on resilience generalist") +
  geom_boxplot(aes(color = cost_plasticity, fill= perturbation), alpha= 0.3, height = 0.5, width=0.2) +
  scale_fill_manual(values=c("10", "20")) + scale_color_manual(values=c("1", "2", "3")) +
  facet_grid( ~fractality, scales="free_y", space="free") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5))# +
 # stat_compare_means()

#tukey

tukey1 <- TukeyHSD(modelgeneral1, c("shannon.dists$level_plasticity", "shannon.dists$cost_plasticity", "shannon.dists$fractality", "shannon.dists$perturbation"))
tukey2 <- TukeyHSD(modelprey1, c("shannon.dists$level_plasticity", "shannon.dists$cost_plasticity", "shannon.dists$fractality", "shannon.dists$perturbation"))
tukey3 <- TukeyHSD(modelpredator1, c("shannon.dists$level_plasticity", "shannon.dists$cost_plasticity", "shannon.dists$fractality", "shannon.dists$perturbation"))
tukey4 <- TukeyHSD(modelspecialist1, c("shannon.dists$level_plasticity", "shannon.dists$cost_plasticity", "shannon.dists$fractality", "shannon.dists$perturbation"))
tukey5 <- TukeyHSD(modelspecialistandgeneralist1, c("shannon.dists$level_plasticity", "shannon.dists$cost_plasticity", "shannon.dists$fractality", "shannon.dists$perturbation"))
tukey6 <- TukeyHSD(modelgeneralist1, c("shannon.dists$level_plasticity", "shannon.dists$cost_plasticity", "shannon.dists$fractality", "shannon.dists$perturbation"))

tukeydifferent1 <- HSD.test(modelgeneral1, c("shannon.dists$level_plasticity", "shannon.dists$cost_plasticity", "shannon.dists$fractality", "shannon.dists$perturbation"))
tukeydifferent2 <- HSD.test(modelprey1, c("shannon.dists$level_plasticity", "shannon.dists$cost_plasticity", "shannon.dists$fractality", "shannon.dists$perturbation"))
tukeydifferent3 <- HSD.test(modelpredator1, c("shannon.dists$level_plasticity", "shannon.dists$cost_plasticity", "shannon.dists$fractality", "shannon.dists$perturbation"))
tukeydifferent4 <- HSD.test(modelspecialist1, c("shannon.dists$level_plasticity", "shannon.dists$cost_plasticity", "shannon.dists$fractality", "shannon.dists$perturbation"))
tukeydifferent5 <- HSD.test(modelspecialistandgeneralist1, c("shannon.dists$level_plasticity", "shannon.dists$cost_plasticity", "shannon.dists$fractality", "shannon.dists$perturbation"))
tukeydifferent6 <- HSD.test(modelgeneralist1, c("shannon.dists$level_plasticity", "shannon.dists$cost_plasticity", "shannon.dists$fractality", "shannon.dists$perturbation"))


#Normality
#shapiro.test(shannon.dists$level_plasticity)

#Homogeinity
leveneTest(shannon.dists$Shannon.dists, shannon.dists$level_plasticity)

