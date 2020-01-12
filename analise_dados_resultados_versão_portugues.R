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

#Loading data
dados <- read.csv(here("Resultado_dados/Dados Brutos/dados_brutos_shannon_dist.csv"), header = TRUE, sep = ";", quote = "\"", dec = ",")
#dadoscombinacoes <- read.csv(here("Resultado_dados/Dados Brutos/Dados brutos para 24 combinacoes/2LowPlasticityLowCostLowPerturbationHighfractality.csv"), header = TRUE, sep = ";", quote = "\"", dec = ",")

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
shannon.dists$cost_plasticity <- factor(shannon.dists$cost_plasticity, levels = c("low", "high"))
shannon.dists$perturbation <- factor(shannon.dists$perturbation, levels = c("low", "high"))
shannon.dists$fractality <- factor(shannon.dists$fractality, levels = c("low", "high"))
shannon.dists$Shannon.dists = shannon.dists$Shannon.dists * -1

#ANOVA general
modelgeneral1 = aov(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity +shannon.dists$fractality + shannon.dists$perturbation )
summary(modelgeneral1)
modelgeneral2 = lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity +shannon.dists$fractality + shannon.dists$perturbation )
summary(modelgeneral2)
kruskal.test(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity)
#plot(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity + shannon.dists$fractality + shannon.dists$perturbation, ylab = "Resilience", ylim = c(-1.3, 0))
plot(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity, main = "Effect of plasticity on resilience", xlab = "Plasticity", ylab = "Resilience", ylim = c(-1.3, 0), cex.main = 1.5, cex.lab = 1.4)
plot(shannon.dists$Shannon.dists ~ shannon.dists$cost_plasticity, main = "Effect of cost on resilience", xlab = "Cost Plasticity", ylab = "Resilience", ylim = c(-1.3, 0), cex.main = 1.5, cex.lab = 1.4)
plot(shannon.dists$Shannon.dists ~ shannon.dists$fractality, main = "Effect of Fractality on resilience", xlab = "Fractality", ylab = "Resilience", ylim = c(-1.3, 0), cex.main = 1.5, cex.lab = 1.4)
plot(shannon.dists$Shannon.dists ~ shannon.dists$perturbation, main = "Effect of Disturbance on resilience", xlab = "Disturbance", ylab = "Resilience", ylim = c(-1.3, 0), cex.main = 1.5, cex.lab = 1.4)

disturbance <- shannon.dists$perturbation

#ANOVA GENERAL TYPE 1
ANOVAGENERAL <- ggplot(data = shannon.dists, aes(x= level_plasticity, y= Shannon.dists)) +
  ggtitle("Effect of plasticity on resilience") +
  geom_boxplot (aes(fill = disturbance), alpha= 0.3) +
  scale_fill_manual(values=c("10", "20")) + scale_color_manual(values=c("1", "2", "3")) +
  #facet_grid( ~fractality, scales="free_y", space="free") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) #+
# stat_compare_means()
#annotate("text", x = 2.2, y = 1.20, label = "P value: <2e-16 (all factors), R:0.80", color = "blue", size = 3) #+ rremove("grid") 
ANOVAGENERAL + scale_x_discrete(name = "Plasticity") + scale_y_continuous(name = "Resilience", limits = c(-1.20, 0)) 

#ANOVA GENERAL TYPE 2
ANOVAGENERAL <- ggplot(data = shannon.dists, aes(x= level_plasticity, y= Shannon.dists)) +
  ggtitle("Effect of plasticity on resilience") +
  geom_boxplot (aes(fill = cost_plasticity), alpha= 0.3) +
  #geom_boxplot (aes(fill = disturbance), alpha= 0.3) +
  scale_fill_manual(values=c("10", "20")) + scale_color_manual(values=c("1", "2", "3")) +
  facet_grid( fractality ~ perturbation, scales="free_y", space="free") + 
  #facet_wrap(~cost_plasticity)
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) #+
  # stat_compare_means()
  #annotate("text", x = 2.2, y = 1.20, label = "P value: <2e-16 (all factors), R:0.80", color = "blue", size = 3) #+ rremove("grid") 
ANOVAGENERAL + scale_x_discrete(name = "Plasticity") + scale_y_continuous(name = "Resilience", limits = c(-1.20, 0)) 

#ANOVA prey and predator
shannondistpreyandpredator   <- data.frame(shannon_dist = c(shannon.dists$shannonprey.dists, shannon.dists$shannonpredator.dists), 
                                           typeanimal = c(1:56000))
shannondistpreyandpredator$typeanimal[1:28000] = c("herbivore")
shannondistpreyandpredator$typeanimal[28001:56000] = c("carnivore")

shannondistpreyandpredator$shannon_dist = shannondistpreyandpredator$shannon_dist * -1
shannondistpreyandpredator$typeanimal <- factor(shannondistpreyandpredator$typeanimal, levels = c("herbivore", "carnivore"))

modelpreyandpredator <- wilcox.test(shannondistpreyandpredator$shannon_dist ~ shannondistpreyandpredator$typeanimal)
summary(modelpreyandpredator)
plot(shannondistpreyandpredator$shannon_dist ~ shannondistpreyandpredator$typeanimal)


ANOVAPREYANDPREDATOR <- ggplot(data = shannondistpreyandpredator, aes(x= typeanimal, y= shannon_dist)) +
  ggtitle("Resilience in herbivore and carnivore") +
  geom_boxplot ( alpha= 0.3) +
  scale_fill_manual(values=c("10", "20")) + scale_color_manual(values=c("1", "2", "3")) +
  #facet_grid( ~fractality, scales="free_y", space="free") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) #+
# stat_compare_means()
#annotate("text", x = 2.2, y = 1.20, label = "P value: <2e-16 (all factors), R:0.80", color = "blue", size = 3) #+ rremove("grid") 
ANOVAPREYANDPREDATOR + scale_x_discrete(name = "Trophic Level") + scale_y_continuous(name = "Resilience", limits = c(-1.20, 0))


#ANOVA specialist, specialistgeneralist, generalist
shannondistspecialization   <- data.frame(shannon_dist = c(shannon.dists$shannonspecialist.dists, shannon.dists$shannonspecialistandgeneralist.dists, shannon.dists$shannongeneralist), 
                                           specialization = c(1:84000))
shannondistspecialization$specialization[1:28000] = c("Specialist")
shannondistspecialization$specialization[28001:56000] = c("SpecieIntermediary")
shannondistspecialization$specialization[56001:84000] = c("Generalist")

shannondistspecialization$shannon_dist = shannondistspecialization$shannon_dist * -1
shannondistspecialization$specialization <- factor(shannondistspecialization$specialization, levels = c("Specialist", "SpecieIntermediary", "Generalist" ))

modelspecialization <- kruskal.test(shannondistspecialization$shannon_dist ~ shannondistspecialization$specialization)
summary(modelspecialization)
plot(shannondistspecialization$shannon_dist ~ shannondistspecialization$specialization)


ANOVASPECIALIZATION <- ggplot(data = shannondistspecialization, aes(x= specialization, y= shannon_dist)) +
  ggtitle("Resilience in food specialization") +
  geom_boxplot ( alpha= 0.3) +
  scale_fill_manual(values=c("10", "20")) + scale_color_manual(values=c("1", "2", "3")) +
  #facet_grid( ~fractality, scales="free_y", space="free") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) #+
# stat_compare_means()
#annotate("text", x = 2.2, y = 1.20, label = "P value: <2e-16 (all factors), R:0.80", color = "blue", size = 3) #+ rremove("grid") 
ANOVASPECIALIZATION + scale_x_discrete(name = "Specialization") + scale_y_continuous(name = "Resilience", limits = c(-1.20, 0))


#tukey

tukey1 <- TukeyHSD(modelgeneral1, c("shannon.dists$level_plasticity", "shannon.dists$cost_plasticity", "shannon.dists$fractality", "shannon.dists$perturbation"))
tukey2 <- TukeyHSD(modelspecialization, c("shannondistspecialization$specialization"))

tukeydifferent1 <- HSD.test(modelgeneral1, c("shannon.dists$level_plasticity", "shannon.dists$cost_plasticity", "shannon.dists$fractality", "shannon.dists$perturbation"))

modelgeneral8 <- aov(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$perturbation)
tukeydifferent8 <- HSD.test(modelgeneral8, c("shannon.dists$level_plasticity","shannon.dists$perturbation"))

#Normality

#shapiro.test(shannon.dists$level_plasticity)

#Subsampling and Homogeinity

mysample <- shannon.dists[sample(1:nrow(shannon.dists),2400,
                          replace=FALSE),]
leveneTest(mysample$Shannon.dists, mysample$level_plasticity)
bartlett.test(mysample$Shannon.dists, mysample$level_plasticity)

#Interaction Fractality and perturbation
ggplot(data = shannon.dists, aes(x= fractality , y= Shannon.dists)) +
  ggtitle("Interaction Fractality and perturbation") +
  geom_boxplot(aes(fill = perturbation), alpha= 0.3, height = 0.5, width=0.2) +
  scale_fill_manual(values=c("10", "20")) + scale_color_manual(values=c("1", "2", "3")) +
  #facet_grid( ~fractality, scales="free_y", space="free") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5)) #+
# stat_compare_means()
#annotate("text", x = 2.2, y = 1.20, label = "P value: <2e-16 (all factors), R:0.80", color = "blue", size = 3) #+ rremove("grid") 

#Other graphics
modelprey1 = aov(shannon.dists$shannonprey.dists ~ shannon.dists$level_plasticity )
summary(modelprey1)
modelprey2 = lm(shannon.dists$shannonprey.dists ~ shannon.dists$level_plasticity )
summary(modelprey2)
shannon.dists$shannonprey.dists = shannon.dists$shannonprey.dists * -1
plot(shannon.dists$shannonprey.dists  ~ shannon.dists$level_plasticity,  main = "Effect of plasticity on resilience herbivore", xlab = "Plasticity", ylab = "Resilience herbivore", ylim = c(-1.3, 0), cex.main = 1.5, cex.lab = 1.4)
plot(shannon.dists$shannonprey.dists  ~ shannon.dists$perturbation, main = "Effect of disturbance on resilience herbivore", xlab = "Disturbance", ylab = "Resilience herbivore", ylim = c(-1.3, 0), cex.main = 1.5, cex.lab = 1.4)

modelpredator1 = aov(shannon.dists$shannonpredator.dists ~ shannon.dists$level_plasticity )
summary(modelpredator1)
modelpredator2 = lm(shannon.dists$shannonpredator.dists ~ shannon.dists$level_plasticity  )
summary(modelpredator2)
shannon.dists$shannonpredator.dists = shannon.dists$shannonpredator.dists * -1
plot(shannon.dists$shannonpredator.dists  ~ shannon.dists$level_plasticity,  main = "Effect of plasticity on resilience carnivore", xlab = "Plasticity", ylab = "Resilience carnivore", ylim = c(-1.3, 0), cex.main = 1.5, cex.lab = 1.4)
plot(shannon.dists$shannonpredator.dists  ~ shannon.dists$perturbation, main = "Effect of disturbance on resilience carnivore", xlab = "Disturbance", ylab = "Resilience carnivore", ylim = c(-1.3, 0), cex.main = 1.5, cex.lab = 1.4)

#ANOVA specialist, specialistgeneralist, generalist

modelspecialist1 = aov(shannon.dists$shannonspecialist.dists ~ shannon.dists$level_plasticity )
summary(modelspecialist1)
modelspecialist2 = lm(shannon.dists$shannonspecialist.dists ~ shannon.dists$level_plasticity  )
summary(modelspecialist2)
shannon.dists$shannonspecialist.dists = shannon.dists$shannonspecialist.dists * -1
plot(shannon.dists$shannonspecialist.dists  ~ shannon.dists$level_plasticity,  main = "Effect of plasticity on resilience specialist", xlab = "Plasticity", ylab = "Resilience specialist", ylim = c(-1.3, 0), cex.main = 1.5, cex.lab = 1.4)
plot(shannon.dists$shannonspecialist.dists  ~ shannon.dists$perturbation, main = "Effect of disturbance on resilience specialist", xlab = "Disturbance", ylab = "Resilience specialist", ylim = c(-1.3, 0), cex.main = 1.5, cex.lab = 1.4)

modelgeneralist1 = aov(shannon.dists$shannongeneralist ~ shannon.dists$level_plasticity )
summary(modelgeneralist1)
modelgeneralist2 = lm(shannon.dists$shannongeneralist ~ shannon.dists$level_plasticity )
summary(modelgeneralist2)
shannon.dists$shannongeneralist = shannon.dists$shannongeneralist * - 1
plot(shannon.dists$shannongeneralist  ~ shannon.dists$level_plasticity,  main = "Effect of plasticity on resilience generalist", xlab = "Plasticity", ylab = "Resilience generalist", ylim = c(-1.3, 0), cex.main = 1.5, cex.lab = 1.4)
plot(shannon.dists$shannongeneralist  ~ shannon.dists$perturbation, main = "Effect of disturbance on resilience generalist", xlab = "Disturbance", ylab = "Resilience generalist", ylim = c(-1.3, 0), cex.main = 1.5, cex.lab = 1.4)

ANOVASSS <- ggplot(data = shannon.dists, aes(x= level_plasticity, y= shannongeneralist  )) +
  ggtitle("Resilience in food specialization") +
  geom_boxplot (aes(fill=disturbance), alpha= 0.3) +
  scale_fill_manual(values=c("10", "20")) + scale_color_manual(values=c("1", "2", "3")) +
  #facet_grid( ~fractality, scales="free_y", space="free") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) #+
# stat_compare_means()
#annotate("text", x = 2.2, y = 1.20, label = "P value: <2e-16 (all factors), R:0.80", color = "blue", size = 3) #+ rremove("grid") 
ANOVASSS + scale_x_discrete(name = "Plasticity") + scale_y_continuous(name = "Resilience generalist", limits = c(-1.20, 0))


#### ALTERNATIVE STATES ########

#plot - Pre e pos perturbacao de cada uma das 24 combinacao de parametros nas variaveis Shannon e equabilidade
#plot(dadoscombinacoes$Shannon[2:1001], dadoscombinacoes$Shannon[1002:2001], main = "Shannon pre and post", xlab = "Shannon pre-disturbance", ylab = "Shannon post-disturbance", xlim = c(0.90, 2.25),ylim = c(0.90, 2.25), col = dadoscombinacoes$ticks)
#plot(dadoscombinacoes$replicate.number, dadoscombinacoes$Shannon, main = "Shannon pre and post", sub = "Red = pre-disturbance, Black = post-disturbance", xlab = "Replicate Number", ylab = "Shannon Value", ylim = c(0.90, 2.25), col = dadoscombinacoes$ticks)

#Kernel Analysis specific - Alternative States
KernelAnalysis <-  ggplot(dadoscombinacoes, aes(dadoscombinacoes$Shannon, fill = ticks)) +
  geom_density(alpha = 0.3) + 
  ggtitle("Kernel analysis") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) #+
KernelAnalysis + scale_x_continuous(name = "Shannon", limits = c(1.0, 2.30)) + scale_y_continuous(name = "Density", limits = c(0.0, 15.0))


#HexbinSpecific - Alternative States
opa <- data.frame("ShannonPre" = dadoscombinacoes$Shannon[1:1000],
                  "ShannonPos" = dadoscombinacoes$Shannon[1001:2000])
densityplot <- ggplot(opa, aes(ShannonPre, ShannonPos )) +
  stat_binhex() + ggtitle("Hexbin analysis general") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) #+
densityplot + scale_x_continuous(name = "ShannonPre", limits = c(1.0, 2.30)) + scale_y_continuous(name = "ShannonPos", limits = c(1.0, 2.30)) 

#Kernel Analysis specific - Alternative States
KernelAnalysis <-  ggplot(dadoscombinacoes, aes(dadoscombinacoes$Shannon, fill = ticks)) +
  geom_density(alpha = 0.3) + 
  #ggtitle("Kernel analysis general") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) #+
KernelAnalysis + scale_x_continuous(name = "Shannon", limits = c(1.0, 2.30)) + scale_y_continuous(name = "Densidade")

#HexBin General - Alternative States
opa2 <- data.frame("ShannonPre" = dadoscombinacoes$Shannon[1:28000],
                   "ShannonPos" = dadoscombinacoes$Shannon[28001:56000])
densityplot <- ggplot(opa2, aes(ShannonPre, ShannonPos )) +
  stat_binhex() + ggtitle("Hexbin analysis general") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) #+
densityplot + scale_x_continuous(name = "Shannon", limits = c(1.0, 2.30)) + scale_y_continuous(name = "Density", limits = c(1.0, 2.30)) 

#View(opa)
#ggplot(opa2, aes(ShannonPre, ShannonPos )) +
# geom_bin2d( bins = 70 ) +
# scale_fill_continuous(type = "viridis") +
# theme_bw()

#Kernel Analysis - principal graphics
KernelAnalysis <-  ggplot(shannon.dists, aes(Shannon.dists, fill = fractality)) +
  geom_density(alpha = 0.3) + 
  ggtitle("Kernel analysis") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) #+
KernelAnalysis + scale_x_continuous(name = "Resilience") + scale_y_continuous(name = "Density")