#////////////////
# Preparing COBP dataset for analysis
# Alice Stears
# 3 November 2021
#///////////////
## load COBP demographic data
## remove data that isn't necessary (notes, leaf spots, invert, stem #s, #capsules, bolting)
butterfly_temp <- read.csv("../../Oenothera coloradensis project/Raw Data/COBP_data_08_23_21.csv") %>% 
  select(c("Location", "Site", "Plot_ID", "Quadrant", "ID", "X_cm", "Y_cm", "LongestLeaf_cm_2018", "Alive_2018", "LongestLeaf_cm_2019", "Alive_2019", "LongestLeaf_cm_2020", "Alive_2020", "Flowering_2018", "Flowering_2019", "Flowering_2020", "No_Capsules_2018", "No_Capsules_2019", "No_Capsules_2020", "Stem_Herbivory_2018", "Stem_Herbivory_2019", "Stem_Herbivory_2020", "Invert_Herbivory_2018", "Invert_Herbivory_2019", "Invert_Herbivory_2020", "LeafSpots_2018", "LeafSpots_2019", "LeafSpots_2020"))

## remove messed up rows (for now...)
# butterfly_temp$index <- paste0(butterfly_temp$Plot_ID,"_", butterfly_temp$ID)
# butterfly_temp <- butterfly_temp[!(butterfly_temp$index %in% c("S6_69", "S6_70", "S6_71", "S6_72", "S8_#REF!")),]
# butterfly_temp <- select(butterfly_temp, !c("index")) 

## change the format of the data.frame from wide to long
butterfly_1 <- butterfly_temp[, c(1:8, 10, 12)] %>% 
  pivot_longer(cols = c("LongestLeaf_cm_2018", "LongestLeaf_cm_2019",  "LongestLeaf_cm_2020"), names_to = "Year", values_to = "LongestLeaf_cm")
butterfly_1$Year <- as.integer(str_extract_all(butterfly_1$Year, pattern = "[:digit:]+", simplify = TRUE))

butterfly_2 <- butterfly_temp[,c(1:7, 9, 11, 13)] %>% 
  pivot_longer(cols = c("Alive_2018", "Alive_2019",  "Alive_2020"), names_to = "Year", values_to = "survives_t")
butterfly_2$Year <- as.integer(str_extract_all(butterfly_2$Year, pattern = "[:digit:]+", simplify = TRUE))

butterfly_3 <- butterfly_temp[,c(1:7, 14:16)] %>% 
  pivot_longer(cols = c("Flowering_2018", "Flowering_2019",  "Flowering_2020"), names_to = "Year", values_to = "flowering")
butterfly_3$Year <- as.integer(str_extract_all(butterfly_3$Year, pattern = "[:digit:]+", simplify = TRUE))

butterfly_4 <- butterfly_temp[,c(1:7, 17:19)] %>% 
  pivot_longer(cols = c("No_Capsules_2018", "No_Capsules_2019",  "No_Capsules_2020"), names_to = "Year", values_to = "Num_capsules")
butterfly_4$Year <- as.integer(str_extract_all(butterfly_4$Year, pattern = "[:digit:]+", simplify = TRUE))

butterfly_5 <- butterfly_temp[,c(1:7, 20:22)] %>% 
  pivot_longer(cols = c("Stem_Herbivory_2018", "Stem_Herbivory_2019",  "Stem_Herbivory_2020"), names_to = "Year", values_to = "Stem_Herb")
butterfly_5$Year <- as.integer(str_extract_all(butterfly_5$Year, pattern = "[:digit:]+", simplify = TRUE))

butterfly_6 <- butterfly_temp[,c(1:7, 23:25)] %>% 
  pivot_longer(cols = c("Invert_Herbivory_2018", "Invert_Herbivory_2019",  "Invert_Herbivory_2020"), names_to = "Year", values_to = "Invert_Herb")
butterfly_6$Year <- as.integer(str_extract_all(butterfly_6$Year, pattern = "[:digit:]+", simplify = TRUE))

butterfly_7 <- butterfly_temp[,c(1:7, 26:28)] %>% 
  pivot_longer(cols = c("LeafSpots_2018", "LeafSpots_2019",  "LeafSpots_2020"), names_to = "Year", values_to = "LeafSpots")
butterfly_7$Year <- as.integer(str_extract_all(butterfly_7$Year, pattern = "[:digit:]+", simplify = TRUE))

butterfly <- left_join(butterfly_1, butterfly_2) %>% 
  left_join(butterfly_3) %>% 
  left_join(butterfly_4) %>% 
  left_join(butterfly_5) %>% 
  left_join(butterfly_6) %>% 
  left_join(butterfly_7)

## make sure that the 'flowering' column is correct
butterfly[which(butterfly$survives_t==1 & is.na(butterfly$flowering)),"flowering"]$flowering <- 0

## generate a survives_tplus1 column 
butterfly$survives_tplus1 <- NA
## make a column that contains size in previous year
butterfly$longestLeaf_tminus1 <- NA
butterfly$longestLeaf_tplus1 <- NA
butterfly$age <- NA
butterfly$seedling <- 0
## give the survival status from the next year to the current year
## assign an arbitrary index to each row in 'butterfly' 
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
    ## make sure the data are from sequential years
    if (sum((c(temp_1$Year, NA) - c(NA, temp_1$Year) != 1), na.rm = TRUE)!= 0) {
      stop("There is a year of data missing for individual ", 
           unique(paste(temp_1$Plot_ID,temp_1$ID, sep = "_")))
    }
    ## make sure that there is data for more than one year (otherwise just data 
    # for 2020 exists, which already has an NA for survival and size_t-1)
    ## if there is an NA in the 'survives_t column? if so, remove that obs.
    temp_1 <- temp_1[is.na(temp_1$survives_t) == FALSE,]
    ## For 'first year' obs (in 2019 and 2020), make an obs. from the year before that is a seedling 
    if (as.integer(temp_1[1,"Year"]) != 2018) {
      ## make a new row for the seedling
      seedling <- temp_1[1,]
      ## make year the previous year
      seedling$Year <- as.integer(temp_1[1,"Year"] - 1)
      seedling$LongestLeaf_cm <- round(runif(n = 1, min = .1, max = 3),2)
      seedling[,c("survives_t", "survives_tplus1", "seedling")] <- 1
      seedling$flowering <- NA
      seedling[,c("Num_capsules", "Stem_Herb", "Invert_Herb", "LeafSpots", "longestLeaf_tminus1")] <- NA
      ## add the new row to the existing data
      temp_1 <- rbind(seedling, temp_1)
    }
    if (nrow(temp_1) > 1) {
      ## get survival data
      ## get a vector of the survival values
      surv <- temp_1$survives_t
      ## remove the first value (don't care if it is alive in the first year)
      survNew <- c(surv[2:length(surv)], NA)
      temp_1$survives_tplus1 <- survNew 
      ## get size_t-1 data
      size <- temp_1$LongestLeaf_cm
      sizeNew <- c(NA, size[1:(length(size)-1)])
      temp_1$longestLeaf_tminus1 <- sizeNew
      ## get size_t+1 data
      size_2 <- temp_1$LongestLeaf_cm
      size_2New <- c(size_2[2:length(size_2)], NA)
      temp_1$longestLeaf_tplus1 <- size_2New
      ## get age data (only for those that aren't established in 2018, for which we don't know age)
      if (temp_1[1,"Year"] == 2018 & 
          temp_1[1,"seedling"] == 0) {
        temp_1$age <- NA
      } else {
        age <- seq(from = 0, to = (nrow(temp_1)-1), by = 1)
        temp_1$age <- age
      }
    } else {
      temp_1$age <- 0
    }
    if (exists("datOut") == FALSE) {
      datOut <- temp_1
    } else {
      datOut <- rbind(datOut, temp_1)
    }
  }
}

butterfly <- datOut
## remove plants that aren't alive in year t
butterfly <- butterfly[butterfly$survives_t != 0 & is.na(butterfly$survives_t) == FALSE,]
## temporarily remove bad rows (missing leaf size data!)
# butterfly <- butterfly[is.na(butterfly$LongestLeaf_cm) == FALSE,]
butterfly$age <- as.integer(butterfly$age)

## assign additional seedling data
## load seedling data
seeds <- read.csv("../Raw Data/COBP_seedlings_8_23_21.csv")
## trim away unnecessary data from seeds d.f
seeds <- seeds %>%
  select(Location, Site, Plot_ID, Quadrant, Seedlings_18, Seedlings_19, Seedlings_20) %>%
  pivot_longer(cols = c("Seedlings_18", "Seedlings_19", "Seedlings_20"), names_to = "Year", values_to = "seedlings_count")
## correct year values
seeds$Year <- as.integer(str_extract_all(seeds$Year, pattern = "[:digit:]+", simplify = TRUE)) + 2000
seeds$ID <- "sdlng"
seeds$X_cm <- NA
seeds$Y_cm <- NA
seeds$survives_t <- NA
seeds$flowering <- NA
seeds$Num_capsules <- NA
seeds$Stem_Herb <- NA
seeds$Invert_Herb <- NA
seeds$LeafSpots <- NA
seeds$survives_tplus1 <- NA
seeds$longestLeaf_tminus1 <- NA  
seeds$longestLeaf_tplus1 <- NA
seeds$index <- NA
seeds$age <- 0               
seeds$log_LL_tplus1 <- NA
seeds$log_LL_tminus1 <- NA 

## make a table of the seedling counts added previously to 'butterfly'
seedTable <- butterfly %>% 
  filter(seedling == 1) %>% 
  group_by(Plot_ID, Quadrant, Year) %>% 
  summarise(seedlingCount = n())

seedsTemp <- left_join(seeds, seedTable)
seedsTemp[is.na(seedsTemp$seedlingCount),"seedlingCount"] <- 0
seeds$seedlings_count <- seedsTemp$seedlings_count - seedsTemp$seedlingCount
## if there is a negative number, make it a zero
seeds[seeds$seedlings_count < 0,"seedlings_count"] <- 0
seeds$survives_tplus1 <- 0
seeds$seedling <- 1

## make repeating rows for each quadrant for the no. of seedlings
seeds <- as.data.frame(lapply(seeds, rep, seeds$seedlings_count))

## use a uniform distribution to randomly assign sizes to the seedlings
seeds$LongestLeaf_cm <- round(runif(n = nrow(seeds), min = .1, max = 3),2)
seeds$log_LL_t <- log(seeds$LongestLeaf_cm) 

## remove the 'seeedlings_count column' and reorder columns
seeds <- seeds %>% 
  select(-seedlings_count) %>% 
  select(names(butterfly))

## add seedlings to other data
butterfly <- rbind(butterfly, seeds)

## write this long-form data to file
write.csv(butterfly, file = "../Raw Data/COBP_long_CURRENT.csv", row.names = FALSE)
