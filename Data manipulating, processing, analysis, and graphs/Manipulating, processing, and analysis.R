# install packages
# install.packages("stats")
# install.packages("olsrr"")
# install.packages("ggplot2")
# install.packages("agricolae")
# install.packages("FactorMineR")
# install.packages("dplyr")
# install.packages("vegan")
# install.packages("agricolae")
# install.packages("ggpubr")
# install.packages("hexbin")
# install.packages("hrbrthemes")
# install.packages("viridis")
# install.packages("olsrr")
# install.packages("here")
# install.packages("MASS")
# install.packages("lmtest")

# carrying packages

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

## Little World ##

#dados <- read.csv(("Usados/dados_brutos_pequeno_mundo_shannon_dist.csv"), header = TRUE, sep = ";", quote = "/", dec = ".")

## Normal World ##
#dados <- read.csv(("Usados/dados_brutos_shannon_dist.csv"), header = TRUE, sep = ";", quote = "/", dec = ".")
#View(dados)

## Big World ##
setwd("C:/Users/Emerson Júnior/Dropbox/Codigos/MestradoEmerson-master/MestradoEmerson-master/MestradoEmerson/Final_analysis")
dados <- read.csv(("Usados/dados_brutos_grande_mundo_shannon_dist.csv"), header = TRUE, sep = ";", quote = "/", dec = ".")
#View(dados)

# Manipuling data

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


dados$cost_plasticity_sheep <- as.numeric(gsub(",", ".", gsub("\\.", "", dados$cost_plasticity_sheep)))

dados$cost_plasticity[dados$cost_plasticity_sheep == 0.2] = "Low"
dados$cost_plasticity[dados$cost_plasticity_sheep == 0.8] = "High"

dados$level_plasticity[dados$sheep_plasticity == 2] ="Low"
dados$level_plasticity[dados$sheep_plasticity == 5] ="Medium"
dados$level_plasticity[dados$sheep_plasticity == 8] ="High"

# calculate shannon trophical level and specialization
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
#as.numeric(dados$Shannon)
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
#View(shannon.dists)
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

shannon.dists$level_plasticity <- factor(shannon.dists$level_plasticity, levels = c("Low", "Medium", "High")) 
shannon.dists$cost_plasticity <- factor(shannon.dists$cost_plasticity, levels = c("Low", "High"))
shannon.dists$perturbation <- factor(shannon.dists$perturbation, levels = c("low", "high"))
shannon.dists$fractality <- factor(shannon.dists$fractality, levels = c("low", "high"))
shannon.dists$Shannon.dists = shannon.dists$Shannon.dists * -1

# Analysis and Graphs

one <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity)
#plot(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity)
summary(one)

two <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity)
summary(two)

three <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$perturbation)
summary(three)

four <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$fractality)
summary(four)

five <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$cost_plasticity + shannon.dists$fractality)
summary(five)

six <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$perturbation + shannon.dists$cost_plasticity)
summary(six)

seven <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$perturbation + shannon.dists$fractality)
summary(seven)

eight <- lm(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$perturbation + shannon.dists$cost_plasticity + shannon.dists$fractality)
summary(eight)

nine <- lm(shannon.dists$Shannon.dists ~ 1)
summary(nine)

#levels(shannon.dists$cost_plasticity)
#droplevels(shannon.dists$level_plasticity)


#dados <- read.csv(("dados_brutos_grande_mundo_shannon_dist.csv"), header = TRUE, sep = ";", quote = "/", dec = ",")
#shannon.dists <- shannon.dists[c(401:2800),]
#View(shannon.dists)

AIC(one)
AIC(two)
AIC(three)
AIC(four)
AIC(five)
AIC(six)
AIC(seven)
AIC(eight)
AIC(nine)

result <- c(AIC(one),
            AIC(two),
            AIC(three),
            AIC(four),
            AIC(five),
            AIC(six),
            AIC(seven),
            AIC(eight),
            AIC(nine))

result2 <- sort(result, decreasing = FALSE)
better_model <- result2[1]
better_model

summary(eight)

#### BETTER MODEL IN COMMUNITY CLASSIFICATION ####

### SPECIALIZATION ###
shannondistspecialization   <- data.frame(shannon_dist = c(shannon.dists$shannonspecialist.dists, shannon.dists$shannonspecialistandgeneralist.dists, shannon.dists$shannongeneralist), specialization = c(1:7200))


shannondistspecialization$specialization[1:2400] = c("Specialist")
shannondistspecialization$specialization[2401:4800] = c("SpecieIntermediary")
shannondistspecialization$specialization[4801:7200] = c("Generalist")

shannondistspecialization$shannon_dist = shannondistspecialization$shannon_dist * -1
shannondistspecialization$specialization <- factor(shannondistspecialization$specialization, levels = c("Specialist", "SpecieIntermediary", "Generalist" ))

modelspecialization <- lm(shannondistspecialization$shannon_dist ~ shannondistspecialization$specialization)

summary(modelspecialization)
plot(shannondistspecialization$shannon_dist ~ shannondistspecialization$specialization)

model1 <- AIC(modelspecialization)
model1

### TROPHIC LEVEL ###

shannondistpreyandpredator   <- data.frame(shannon_dist = c(shannon.dists$shannonprey.dists, shannon.dists$shannonpredator.dists), 
                                           typeanimal = c(1:4800))

shannondistpreyandpredator$typeanimal[1:2400] = c("herbivore")
shannondistpreyandpredator$typeanimal[2401:4800] = c("carnivore")

shannondistpreyandpredator$shannon_dist = shannondistpreyandpredator$shannon_dist * -1
shannondistpreyandpredator$typeanimal <- factor(shannondistpreyandpredator$typeanimal, levels = c("herbivore", "carnivore"))

modelpreypredator <- lm(shannondistpreyandpredator$shannon_dist ~ shannondistpreyandpredator$typeanimal)
summary(modelpreypredator)
#plot(shannondistpreyandpredator$shannon_dist ~ shannondistpreyandpredator$typeanimal)

model2 <- AIC(modelpreypredator)
model2
result <- c(model1, model2)
sorting <- sort(result, decreasing = FALSE)
final_result <- sorting[1]
final_result



### TUKEY TEST ###

TukeyHSD(aov(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity))

TukeyHSD(aov(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity * shannon.dists$perturbation))

TukeyHSD(aov(shannon.dists$Shannon.dists ~ shannon.dists$perturbation))

TukeyHSD(aov(shannon.dists$Shannon.dists ~ shannon.dists$cost_plasticity))

TukeyHSD(aov(shannon.dists$Shannon.dists ~ shannon.dists$fractality))

TukeyHSD(aov(shannondistpreyandpredator$shannon_dist ~ shannondistpreyandpredator$typeanimal))

TukeyHSD(aov(shannondistspecialization$shannon_dist ~ shannondistspecialization$specialization))
plot(shannondistspecialization$shannon_dist ~ shannondistspecialization$specialization)


### EXTRA ###
plasticity <- summary(aov(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity))
plasticity
plot(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity)


plasticityanddisturbance <- summary(aov(shannon.dists$Shannon.dists ~ shannon.dists$level_plasticity + shannon.dists$perturbation))
plasticityanddisturbance

cost <- t.test(shannon.dists$Shannon.dists ~ shannon.dists$cost_plasticity)
cost
boxplot(shannon.dists$Shannon.dists ~ shannon.dists$cost_plasticity)

disturbanceextension <- t.test(shannon.dists$Shannon.dists ~ shannon.dists$perturbation)
disturbanceextension
boxplot(shannon.dists$Shannon.dists ~ shannon.dists$perturbation)

disturbanceform <- t.test(shannon.dists$Shannon.dists ~ shannon.dists$fractality)
disturbanceform

herbivore <- t.test(shannondistpreyandpredator$shannon_dist ~ shannondistpreyandpredator$typeanimal)
herbivore

specialization <- summary(aov(shannondistspecialization$shannon_dist ~ shannondistspecialization$specialization))
specialization

### GRAPHICS ###

disturbance <- shannon.dists$perturbation

### PLASTICITY AND DISTURBANCE####
ANOVAPLASTICITY <- ggplot(data = shannon.dists, aes(x= disturbance, y= Shannon.dists)) +
  geom_violin (width = 0.9) +
  ggtitle("Effect of disturbance on resilience") +
  geom_boxplot (width = 0.1, color = "grey",  alpha = 0.2) +
  scale_fill_viridis(discrete = TRUE) +
  #facet_grid( ~fractality, scales="free_y", space="free") + 
  theme_bw() + theme(plot.title = element_text(size = 12, face = 2, hjust = 0.5), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) #+
# stat_compare_means()
#annotate("text", x = 2.2, y = 1.20, label = "P value: <2e-16 (all factors), R:0.80", color = "blue", size = 3) #+ rremove("grid") 
ANOVAPLASTICITY + scale_x_discrete(name = "Disturbance") + scale_y_continuous(name = "Resilience", limits = c(-1.20, 0))


disturbance <- shannon.dists$perturbation
dodge <- position_dodge(width = 0.7)
Plasticity_Disturbance <- ggplot(data = shannon.dists, aes(x= level_plasticity, y= Shannon.dists, fill = disturbance)) +
  geom_violin (width = 1.0, position = dodge) +
  ggtitle("Effect of plasticity and disturbance on resilience") +
  geom_boxplot (width = 0.7, color = "grey",  alpha = 0.8) 
Final_graphic_1 <- Plasticity_Disturbance + scale_x_discrete(name = "Plasticity") + scale_y_continuous(name = "Resilience", limits = c(-1.20, 0)) + theme_classic() + theme(plot.title = element_text(size = 23, face = 2, hjust = 0.5), axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20))
Final_graphic_1

tiff("Plasticity_Disturbance.tiff",
     width = 22,
     height = 14,
     units = "cm",
     res = 300)
Final_graphic_1
dev.off()

Plasticity <- ggplot(data = shannon.dists, aes(x= level_plasticity, y= Shannon.dists)) +
  geom_violin (width = 1.0, position = dodge, fill = "blue") +
  ggtitle("Effect of plasticity on resilience") +
  geom_boxplot (width = 0.7, color = "gray",  alpha = 0.8, fill = "gray") 
Final_graphic_2 <- Plasticity + scale_x_discrete(name = "Plasticity") + scale_y_continuous(name = "Resilience", limits = c(-1.20, 0)) + theme_classic() + theme(plot.title = element_text(size = 23, face = 2, hjust = 0.5), axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20))
Final_graphic_2

tiff("Plasticity.tiff",
     width = 22,
     height = 14,
     units = "cm",
     res = 300)
Final_graphic_2
dev.off()


### PLASTICITY COST ###
COST <- ggplot(data = shannon.dists, aes(x= cost_plasticity, y= Shannon.dists)) +
  geom_violin (width = 1.0, position = dodge, fill = "blue") +
  ggtitle("Effect of plasticity cost on resilience") +
  geom_boxplot (width = 0.7, color = "gray",  alpha = 0.8, fill = "gray") 
Final_graphic_3 <- COST + scale_x_discrete(name = "Plasticity cost") + scale_y_continuous(name = "Resilience", limits = c(-1.20, 0)) + theme_classic() + theme(plot.title = element_text(size = 23, face = 2, hjust = 0.5), axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20))
Final_graphic_3

tiff("Cost_Plasticity.tiff",
     width = 22,
     height = 14,
     units = "cm",
     res = 300)
Final_graphic_3
dev.off()

### DISTURBANCE EXTENSION ###
DisturbanceExtension <- ggplot(data = shannon.dists, aes(x= perturbation, y= Shannon.dists)) +
  geom_violin (width = 1.0, position = dodge, fill = "blue") +
  ggtitle("Effect of disturbance extension on resilience") +
  geom_boxplot (width = 0.7, color = "gray",  alpha = 0.8, fill = "gray") 
Final_graphic_4 <- DisturbanceExtension + scale_x_discrete(name = "Disturbance Extension") + scale_y_continuous(name = "Resilience", limits = c(-1.20, 0)) + theme_classic() + theme(plot.title = element_text(size = 23, face = 2, hjust = 0.5), axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20))
Final_graphic_4

tiff("DisturbanceExtension.tiff",
     width = 22,
     height = 14,
     units = "cm",
     res = 300)
Final_graphic_4
dev.off()


### DISTURBANCE FORM ###
DisturbanceForm <- ggplot(data = shannon.dists, aes(x= fractality, y= Shannon.dists)) +
  geom_violin (width = 1.0, position = dodge, fill = "blue") +
  ggtitle("Effect of disturbance form on resilience") +
  geom_boxplot (width = 0.7, color = "gray",  alpha = 0.8, fill = "gray") 
Final_graphic_5 <- DisturbanceForm + scale_x_discrete(name = "Disturbance Form") + scale_y_continuous(name = "Resilience", limits = c(-1.20, 0)) + theme_classic() + theme(plot.title = element_text(size = 23, face = 2, hjust = 0.5), axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20))
Final_graphic_5

tiff("DisturbanceForm.tiff",
     width = 22,
     height = 14,
     units = "cm",
     res = 300)
Final_graphic_5
dev.off()

### TROPHIC LEVEL ###
ANOVAPREYANDPREDATOR <- ggplot(data = shannondistpreyandpredator, aes(x = typeanimal, y= shannon_dist)) +
  geom_violin (width = 1.0, position = dodge, fill = "blue") +
  ggtitle("Effect of Trophical Level on resilience") +
  geom_boxplot (width = 0.7, color = "gray",  alpha = 0.8, fill = "gray") 
Final_graphic_6 <- ANOVAPREYANDPREDATOR  + scale_x_discrete(name = "Trophic Level") + scale_y_continuous(name = "Resilience", limits = c(-1.20, 0)) + theme_classic() + theme(plot.title = element_text(size = 23, face = 2, hjust = 0.5), axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20))
Final_graphic_6

tiff("ANOVAPREYANDPREDATOR.tiff",
     width = 22,
     height = 14,
     units = "cm",
     res = 300)
Final_graphic_6
dev.off()

### SPECIALIZATION ###
ANOVASPECIALIZATION <- ggplot(data = shannondistspecialization, aes(x= specialization, y= shannon_dist)) +
  geom_violin (width = 1.0, position = dodge, fill = "blue") +
  ggtitle("Effect of Specialization on resilience") +
  geom_boxplot (width = 0.7, color = "gray",  alpha = 0.8, fill = "gray") 
Final_graphic_7 <- ANOVASPECIALIZATION  + scale_x_discrete(name = "Specialization") + scale_y_continuous(name = "Resilience", limits = c(-1.20, 0)) + theme_classic() + theme(plot.title = element_text(size = 23, face = 2, hjust = 0.5), axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20))
Final_graphic_7

tiff("ANOVASPECIALIZATION.tiff",
     width = 22,
     height = 14,
     units = "cm",
     res = 300)
Final_graphic_7
dev.off()
