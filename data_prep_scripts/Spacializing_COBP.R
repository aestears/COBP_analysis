library(tidyverse)
library(sf)
## load the data from "preppingData.R"
source("./data_prep_scripts/preppingData.R")
## convert the data to an sf format
## first make them points

butterfly<- st_as_sf(butterfly, coords = c("X_cm", "Y_cm"))
## add a buffer with a radius of .5 cm 
butterfly <- st_buffer(butterfly, dist = .5)


## test w/ a plot
ggplot(butterfly[butterfly$Plot_ID == "S9",]) +
  geom_hline(aes(yintercept = c(100)), col = "grey20") +
  geom_hline(aes(yintercept = c(200)), col = "grey20") +
  geom_vline(aes(xintercept = c(100)), col = "grey20") +
  geom_vline(aes(xintercept = c(200)), col = "grey20") +
  geom_sf(aes(fill = as.factor(ID)), alpha = .5) + 
  geom_hline(aes(yintercept = c(100))) +
  geom_hline(aes(yintercept = c(200))) +
  theme_classic()


rm(list = (ls()[c(1,3:21)]))
## save a version of the environment (to avoid the )
save.image(file = "../../Oenothera coloradensis project/Processed_Data/spatial_COBP.RData")
