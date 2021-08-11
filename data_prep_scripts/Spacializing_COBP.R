library(tidyverse)
library(sf)

## load COBP demographic data
## remove data that isn't necessary (notes, leaf spots, invert, stem #s, #capsules, bolting)
butterfly_temp <- read.csv("../Raw Data/COBP_data_10_25_20.csv") %>% 
  select(c("Location", "Site", "Plot_ID", "Quadrant", "ID", "X_cm", "Y_cm", "LongestLeaf_cm_2018", "Alive_2018", "LongestLeaf_cm_2019", "Alive_2019", "LongestLeaf_cm_2020", "Alive_2020"))

## remove messed up rows (for now...)
butterfly_temp$index <- paste0(butterfly_temp$Plot_ID,"_", butterfly_temp$ID)
butterfly_temp <- butterfly_temp[!(butterfly_temp$index %in% c("S6_69", "S6_70", "S6_71", "S6_72", "S8_#REF!")),]
butterfly_temp <- select(butterfly_temp, !c("index")) 

## change the format of the data.frame from wide to long
butterfly_1 <- butterfly_temp[, c(1:8, 10, 12)] %>% 
  pivot_longer(cols = c("LongestLeaf_cm_2018", "LongestLeaf_cm_2019",  "LongestLeaf_cm_2020"), names_to = "Year", values_to = "LongestLeaf_cm")
butterfly_1$Year <- as.integer(str_extract_all(butterfly_1$Year, pattern = "[:digit:]+", simplify = TRUE))

butterfly_2 <- butterfly_temp[,c(1:7, 9, 11, 13)] %>% 
  pivot_longer(cols = c("Alive_2018", "Alive_2019",  "Alive_2020"), names_to = "Year", values_to = "survives_t")
butterfly_2$Year <- as.integer(str_extract_all(butterfly_2$Year, pattern = "[:digit:]+", simplify = TRUE))

butterfly <- left_join(butterfly_1, butterfly_2)

## generate a survives_tplus1 column 
## give an 'NA' to all individuals in 2020 (no sampling in 2021)
butterfly$survives_tplus1 <- NULL
#butterfly[butterfly$Year == 2020, "survives_tplus1"] <- NA
## give the survival status from the next year to the current year
## assing an arbitrary index to each row in 'butterfly' 
butterfly$index <- 1:nrow(butterfly)
rm("datOut")
for (i in unique(butterfly$Plot_ID)) {
  ## get data just for one quad
  temp <- butterfly[butterfly$Plot_ID == i,]
  for (j in unique(temp$ID)) {
    ## get data just for one individual
    temp_1 <- temp[temp$ID == j, ]
    ## sort by year 
    temp_1 <- temp_1[order(temp_1$Year),]
    ## make sure that there is data for more than one year (otherwise just data 
    # for 2020 exists, which already has an NA for survival)
    if (nrow(temp_1) > 1) {
      ## get a vector of the survival values
      surv <- temp_1$survives_t
      ## remove the first value (don't care if it is alive in the first year)
      survNew <- c(surv[2:length(surv)], NA)
      temp_1$survives_tplus1 <- survNew 
      # ## put the data back into 'butterfly' 
      # butterfly[which(butterfly$index == temp_1$index), "survives_tplus1"] <- 
      #   temp_1$survives_tplus1
      if (exists("datOut") == FALSE) {
        datOut <- temp_1
      } else {
        datOut <- rbind(datOut, temp_1)
      }
    }
  }
}

butterfly <- datOut
## remove plants that aren't alive in year t
butterfly <- butterfly[butterfly$survives_t != 0 & is.na(butterfly$survives_t) == FALSE,]
## temporarily remove bad rows (missing leaf size data!)
butterfly <- butterfly[is.na(butterfly$LongestLeaf_cm) == FALSE,]

## convert the data to an sf format
## first make them points
butterfly<- st_as_sf(butterfly, coords = c("X_cm", "Y_cm"))
## add a buffer with a radius that's the size of the longest leaf
butterfly <- st_buffer(butterfly, dist = butterfly$LongestLeaf_cm)


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

## save a version of the spatialized data
write.csv(x = butterfly, file = "../Processed_Data/spatial_COBP.csv", 
          row.names = FALSE)
