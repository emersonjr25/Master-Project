#install.packages("here")
#install.packages("beepr")
library(here)
library(dplyr)
library(data.table) # fread is a faster file reader than read.table
library(forcats)
library(beepr)

getwd()

path <- file.path(here("output"))
files <- list.files(path = path, full.names = T, pattern = ".csv") #só as primeiras 100 réplicas
tmp <- lapply(files, data.table::fread, header = T, sep = ";")
beep()
save(tmp, file = "all.output.RData", compress = TRUE)
#load("all.output.RData")
species <- do.call(rbind, tmp)
rm(tmp)
write.csv2(species,"dados_brutos.csv", row.names = F)

#sheep-plasticity;wolf-plasticity;cost-plasticity-sheep;cost-plasticity-wolf;sheep-gain-from-food;wolf-gain-from-food;wolf-gain-from-food;sheep-reproduce;wolf-reproduce;grass-regrowth-time;model-version;replicate-number;Richness; Shannon; Evenness; AbRelPgre; AbRelPvio; AbRelPgray; AbRelPsky; AbRelSone; AbRelStwo; AbRelSthree; AbRelSfou; AbRelWone; AbRelWtwo; AbRelWthree; AbRelWfour
 summary(species)
species %>% 
   group_by (
            sheep_gain_from_food,
            wolf_gain_from_food,
            sheep_reproduce,
            wolf_reproduce,
            grass_regrowth_time) %>%
   summarize(mean_Richness = mean(Richness , na.rm = TRUE),
             sd_Richness = sd(Richness , na.rm = TRUE),
             mean_Shannon = mean(Shannon , na.rm = TRUE),
             mean_Evenness = mean(Evenness, na.rm = TRUE),
             mean_AbRelPgre = mean(AbRelPgre, na.rm = TRUE),
             mean_AbRelPvio = mean(AbRelPvio, na.rm = TRUE),
             mean_AbRelPgray = mean(AbRelPgray, na.rm = TRUE),
             mean_AbRelPsky = mean(AbRelPsky, na.rm = TRUE),
             mean_AbRelSone = mean(AbRelSone, na.rm = TRUE),
             mean_AbRelSthree = mean(AbRelSthree, na.rm = TRUE),
             mean_AbRelSfou = mean(AbRelSfou, na.rm = TRUE),
             mean_AbRelWone = mean(AbRelWone, na.rm = TRUE),
             mean_AbRelWtwo = mean(AbRelWtwo, na.rm = TRUE),
             mean_AbRelWfour = mean(AbRelWfour, na.rm = TRUE),
             reps = n()) -> Richness_per_set
summary(Richness_per_set)
write.csv2(Richness_per_set,"dados_brutos_sintetizados.csv", row.names = F)
