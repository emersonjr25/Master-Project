install.packages("here")
install.packages("beepr")
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

#sheep-plasticity;wolf-plasticity;cost-plasticity-sheep;cost-plasticity-wolf;sheep-gain-from-food;wolf-gain-from-food;wolf-gain-from-food;sheep-reproduce;wolf-reproduce;grass-regrowth-time;sizeimp;replicate-number;Richness; Shannon; Evenness; AbRelPgre; AbRelPvio; AbRelPgray; AbRelPsky; AbRelSone; AbRelStwo; AbRelSthree; AbRelSfou; AbRelWone; AbRelWtwo; AbRelWthree; AbRelWfour

species %>% 
   group_by (ticks,
            sheep_plasticity,
            wolf_plasticity,
            cost_plasticity_sheep,
            cost_plasticity_wolf,
            sheep_gain_from_food,
            wolf_gain_from_food,
            sheep_reproduce,
            wolf_reproduce,
            grass_regrowth_time) %>%
   summarize(mean_Richness = mean(Richness , na.rm = TRUE),
             sd_Richness = sd(Richness , na.rm = TRUE)) -> Richness_per_set

