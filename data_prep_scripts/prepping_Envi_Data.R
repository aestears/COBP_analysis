#//////////////////////////////////////////////////
# COBP environmental data generation/data cleaning
# Alice Stears
# 30 November 2021
#//////////////////////////////////////////////////

library(tidyverse)
# location of data files 
datFolder <- "/Users/alicestears/Dropbox/Grad School/Research/Oenothera coloradensis project"
#### load soil moisture data ####
soilMoist <- read.csv(paste0(datFolder,"/Raw Data/EnviData/SoilMoisture.csv"))
# subset for m3/m3 data
soilMoist <- soilMoist %>% 
  filter(Reading_Type == "m3/m3") %>% 
  group_by(Site, Date) %>% 
  summarize(SoilMoisture_m3m3 = mean(Value))

#### load soil temperature data ####
soilTemp <- read.csv(paste0(datFolder, "/Raw Data/EnviData/iButton Data/SoilTemp_allYear.csv"))
# correct the format of the dat column
soilTemp$Date <- as.POSIXct(x = soilTemp$Date, tz = "UTC")
# add a month column
soilTemp$month <- lubridate::month(soilTemp$Date)
# add NA's for 'missing data'
names(soilTemp)
# S8: no data for months 1-4 and 11-12
S8 <- data.frame("DateTime" = as.character(NA), "SoilTemp" = as.numeric(NA), "Plot" = "S8", "Site" = "Meadow", "Location" = "Soapstone", Date = as.POSIXct(c("2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-11-01", "2019-12-01"), tz = "UTC"), "month"= c(1, 2, 3, 4, 11, 12))
# U3, U4, U6: no data for months 1-4 and 11-12
# there isn't any data for any Unnamed Creek plots for months 1-4 and 11-12; replace with diamond creek averages?
Dtemps <- soilTemp %>% 
  filter(Site == "Diamond Creek") %>% 
  group_by(month) %>% 
  summarize(meanMonthlyTemp = mean(SoilTemp))
U3 <- data.frame("DateTime" = as.character(NA), "SoilTemp" = Dtemps$meanMonthlyTemp[c(1:4,10:11)], "Plot" = "U3", "Site" = "Unnamed Creek", "Location" = "FEWAFB", Date = as.POSIXct(c("2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-11-01", "2019-12-01"), tz = "UTC"), "month"= c(1, 2, 3, 4, 11, 12))
U4 <- data.frame("DateTime" = as.character(NA), "SoilTemp" = Dtemps$meanMonthlyTemp[c(1:4,10:11)], "Plot" = "U4", "Site" = "Unnamed Creek", "Location" = "FEWAFB", Date = as.POSIXct(c("2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-11-01", "2019-12-01"), tz = "UTC"), "month"= c(1, 2, 3, 4, 11, 12))
U6 <- data.frame("DateTime" = as.character(NA), "SoilTemp" = Dtemps$meanMonthlyTemp[c(1:4,10:11)], "Plot" = "U6", "Site" = "Unnamed Creek", "Location" = "FEWAFB", Date = as.POSIXct(c("2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-11-01", "2019-12-01"), tz = "UTC"), "month"= c(1, 2, 3, 4, 11, 12))
# S3: no data for months 1-9
S3 <- data.frame("DateTime" = as.character(NA), "SoilTemp" = as.numeric(NA), "Plot" = "S3", "Site" = "HQ5", "Location" = "Soapstone", Date = as.POSIXct(c("2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-11-01", "2019-12-01"), tz = "UTC"), "month"= c(1, 2, 3, 4, 11, 12))

soilTemp <- rbind(soilTemp, S8, U3, U4, U6, S3)

# calculate growing season average soil temp (April - August) 
soilTemp_grow <- soilTemp %>% 
  filter(month %in% c(4:9)) %>% 
  group_by(Plot, Site, Location) %>% 
  summarize(SoilTemp_grow_C = mean(SoilTemp), sd_soilTemp_grow = sd(SoilTemp))
# calculate it by month 
soilTemp_grow_monthly <- soilTemp %>% 
  filter(month %in% c(4:9)) %>% 
  group_by(Plot, month) %>% 
  summarize(SoilTemp_grow_C = mean(SoilTemp))

# calculate winter average soil temp (October- March) 
soilTemp_winter <- soilTemp %>% 
  filter(month %in% c(1:3,10:12)) %>% 
  group_by(Plot, Site, Location) %>% 
  summarize(SoilTemp_winter_C = mean(SoilTemp), sd_soilTemp_winter = sd(SoilTemp))
# calculate it by month 
soilTemp_winter_monthly <- soilTemp %>% 
  filter(month %in% c(1:3,10:12)) %>% 
  group_by(Plot, month) %>% 
  summarize(SoilTemp_winter_C = mean(SoilTemp))

## plot the soil temperature density by month
ggplot(data = soilTemp) +
  geom_density(aes(SoilTemp, col = as.factor(month))) + 
  xlab(c("Soil Temperature (C)")) +
  theme_classic()
ggplot(data = soilTemp_winter_monthly) + 
  geom_density(aes(SoilTemp_winter_C, col = as.factor(Plot))) + 
  xlab(c("Soil Temperature (C)")) +
  theme_classic()

## get the plot-level mean and sd of soil temp in the growing season and in winter
soilTemp_plot <- left_join(soilTemp_winter, soilTemp_grow)
# there isn't any data for any Unnamed Creek plots for months 1-4 and 11-12; replace with diamond creek averages?

#### load PRISM data ####
baseClim <- read.csv(paste0(datFolder,"/Raw Data/EnviData/FEWAB_PRISM_data.csv"), skip = 10)
names(baseClim) <- c("Date", "precip_in", "tMin_F", "tMean_F", "tMax_F", "meanDewPt_F", "VPDmin_hPa", "VPDmax_hPa")
baseClim$Date <- paste0(baseClim$Date,"-01")
baseClim$Date <- as.POSIXct(x = baseClim$Date, format = "%Y-%m-%d", tz = "UTC")
baseClim$Location <- "FEWAFB"


SoapClim <- read.csv(paste0(datFolder, "/Raw Data/EnviData/Soapstone_PRISM_data.csv"), skip = 10)
names(SoapClim) <- c("Date", "precip_in", "tMin_F", "tMean_F", "tMax_F", "meanDewPt_F", "VPDmin_hPa", "VPDmax_hPa")
SoapClim$Date <- paste0(SoapClim$Date,"-01")
SoapClim$Date <- as.POSIXct(x = SoapClim$Date, format = "%Y-%m-%d", tz = "UTC")
SoapClim$Location <- "Soapstone"

# put both datasets together
Climate <- rbind(baseClim, SoapClim)

# convert to appropriate units
Climate <- Climate %>% 
  mutate(precip_cm = round(precip_in*2.54,2), tMin_C = round(((tMin_F-32)*(5/9)),2),
            tMax_C = round(((tMax_F-32)*(5/9)),2), tMean_C = round(((tMean_F-32)*(5/9)),2),
            meanDewPt_C = round(((meanDewPt_F-32)*(5/9)),2)) %>% 
  dplyr::select(Date, Location, precip_cm, tMin_C, tMax_C, tMean_C, meanDewPt_C, VPDmin_hPa, VPDmax_hPa)

# calculate growing season mean temp/year
climGrow <- Climate %>% 
  filter(lubridate::month(Date) %in% c(4:9)) %>% 
  group_by(Location, lubridate::year(Date)) %>% 
  summarize(tMean_grow_C = mean(tMean_C), sd_tMean_grow = sd(tMean_C)) %>% 
  rename( "Year" = 'lubridate::year(Date)')
# calculate winter mean temp/year
climWinter <- Climate %>% 
  filter(lubridate::month(Date) %in% c(1:3,11:12)) %>% 
  group_by(Location, lubridate::year(Date)) %>% 
  summarize(tMean_winter_C = mean(tMean_C), sd_tMean_winter = sd(tMean_C)) %>% 
  rename("Year" = 'lubridate::year(Date)')
# calculate water-year precip/year
# calculate water year
Climate$waterYear <- NA
Climate[lubridate::year(Climate$Date)==2017 & 
          lubridate::month(Climate$Date) %in% c(1:9),"waterYear"] <- 2017
Climate[(lubridate::year(Climate$Date)==2017 & 
          lubridate::month(Climate$Date) %in% c(10:12)) | 
          (lubridate::year(Climate$Date)==2018 & 
                    lubridate::month(Climate$Date) %in% c(1:9)),
        "waterYear"] <- 2018
Climate[(lubridate::year(Climate$Date)==2018 & 
           lubridate::month(Climate$Date) %in% c(10:12)) | 
          (lubridate::year(Climate$Date)==2019 & 
             lubridate::month(Climate$Date) %in% c(1:9)),
        "waterYear"] <- 2019
Climate[(lubridate::year(Climate$Date)==2019 & 
           lubridate::month(Climate$Date) %in% c(10:12)) | 
          (lubridate::year(Climate$Date)==2020 & 
             lubridate::month(Climate$Date) %in% c(1:9)),
        "waterYear"] <- 2020
Climate[(lubridate::year(Climate$Date)==2020 & 
           lubridate::month(Climate$Date) %in% c(10:12)),"waterYear"] <- 2021

climPrecip <- Climate %>% 
  group_by(Location, waterYear) %>% 
  summarize(precipWaterYr_cm = sum(precip_cm)) %>% 
  filter(waterYear != 2021) %>% 
  rename("Year" = "waterYear")

## add all the climate data together into one d.f.
Climate <- left_join(climGrow, climWinter) %>% 
  left_join(climPrecip)

## remove the spaces in the site names
soilTemp_plot$Site <- str_replace(string = soilTemp_plot$Site, pattern = " ", replacement = "_")

rm(list = c("baseClim", "climGrow", "climPrecip", "climWinter", "Dtemps", "S3", "S8","SoapClim", "soilTemp_grow","soilTemp_grow_monthly",  "soilTemp", "soilTemp_winter", "soilTemp_winter_monthly", "U3","U4","U6"))
