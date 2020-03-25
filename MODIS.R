primary<- readRDS("PREDICTSbirdscorrectnames.rds") %>%
  filter(Sampling_target == "Entire community") %>%
  filter(Predominant_habitat == "Primary forest" |
           Predominant_habitat == "Primary non-forest") #%>%

setwd("C:/Users/tlw119/Dropbox/My drive/PhDizzle/PREDICTS")
library(dplyr) # for easy data manipulation
library(stringr)
library(stringi)
library(tidyr) # ditto
library(magrittr) # for piping
library(lme4) # for mixed effects models
library(car) # for logit transformation with adjustment
library(foreach) # running loops
library(doParallel) # running loops in parallel
library(raster) # for working with raster data
library(geosphere) # calculating geographic distance between sites
library(SYNCSA)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(MODISTools)

primary<- readRDS("PREDICTSbirdscorrectnames.rds") %>%
  filter(Sampling_target == "Entire community") %>%
  filter(Predominant_habitat == "Primary forest" |
           Predominant_habitat == "Primary non-forest") #%>%

#==========================================
#Generating the MODIS pixel Values
#==========================================
#Create a mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#================================================
#MODIS value for treecover and landtype
#The get MODIS function for tree cover
getModisValue <- function(s1) {
  data_ssbs1 <- diversity %>%
    filter(SSBS == s1) %>%
    distinct(SSBS, .keep_all = T)
  data_ssbs1$Sample_start_earliest <- floor_date(data_ssbs1$Sample_start_earliest, "year")
  data_ssbs1$Sample_end_latest <- ceiling_date(data_ssbs1$Sample_end_latest, "year")
  subset <- mt_subset(product = "MOD44B",
                      lat = as.character(data_ssbs1$Latitude),
                      lon = as.character(data_ssbs1$Longitude),
                      band = "Percent_Tree_Cover",
                      start = ifelse(as.Date(data_ssbs1$Sample_start_earliest) < as.Date("2000-01-01"),"2000-03-05", as.character(data_ssbs1$Sample_start_earliest)),
                      end = ifelse(as.Date(data_ssbs1$Sample_end_latest) < as.Date("2000-01-01"),"2000-03-05", as.character(data_ssbs1$Sample_end_latest)),
                      km_lr = 0,
                      km_ab = 0,
                      site_name = "testsite",
                      internal = TRUE,
                      progress = FALSE)
  getmode(subset$value)
}

##the get MODIS function for Land Type
getModisValueLT <- function(s1) {
  data_ssbs1 <- diversity %>%
    filter(SSBS == s1) %>%
    distinct(SSBS, .keep_all = T)
  data_ssbs1$Sample_start_earliest <- floor_date(data_ssbs1$Sample_start_earliest, "year")
  data_ssbs1$Sample_end_latest <- ceiling_date(data_ssbs1$Sample_end_latest, "year")
  subset <- mt_subset(product = "MCD12Q1",
                      lat = as.character(data_ssbs1$Latitude),
                      lon = as.character(data_ssbs1$Longitude),
                      band = "LC_Prop2",
                      start = ifelse(as.Date(data_ssbs1$Sample_start_earliest) < as.Date("2001-01-01"),"2001-01-01", as.character(data_ssbs1$Sample_start_earliest)),
                      end = ifelse(as.Date(data_ssbs1$Sample_end_latest) < as.Date("2001-01-01"),"2001-01-01", as.character(data_ssbs1$Sample_end_latest)),
                      km_lr = 0,
                      km_ab = 0,
                      site_name = "testsite",
                      internal = TRUE,
                      progress = FALSE)
  getmode(subset$value)
}


#================================================
#create a dataframe with the nonforest sites and an empty collumn to insert the pixel values
primarysites <- primary %>%
  distinct(SSBS)
MODISvalueTC <- vector(mode="numeric", length=length(primarysites$SSBS))
siteMODIS <- data.frame(primarysites, MODISvalue)

for (i in 1:length(siteMODIS$SSBS)) {
  siteMODIS$MODISvalueTC[i]<- getModisValueTC(s1 = as.character(siteMODIS$SSBS[i]))
}


siteMODIS$Landtypepixel <- vector(mode="numeric", length=length(primarysites$SSBS))

for (i in 1:length(siteMODIS$SSBS)) {
  siteMODIS$Landtypepixel[i]<- getModisValueLT(s1 = as.character(siteMODIS$SSBS[i]))
}


types <- c("Barren", "Water Bodies", "Urban", "Dense Forest", "Open Forest", "Forest/cropland mosaic", "Natural Herbacious", "Herbacious Cropland", "Shrubland")
MODISLandTypescores <- c(1,3,9,10,20,25,30,36,40)
Landuse <- data.frame(types, MODISLandTypescores)



#--------------------------------------------------------------------------------------------
#I now have a dataframe with all primary sites with their Treecover pixel 
#4 entries are 200 (= water)
#for this reason I have added the Land type score which has a lower resolution 500m pixel (c.f. 250m pixel) I will take the Land-type score for these
#2 of these four erroneous entries are also scored as water in the land use type. For this I will Manually change by inspecting the closest non water pixel in Earth Engine
#---------------------------------------------------------------------------------------------

siteMODIS$MODISvalue[siteMODIS$SSBS == "AD1_2002__Vazquez 1 1 1"] <- 10
siteMODIS$MODISvalue[siteMODIS$SSBS == "JD1_2010__Sodhi 1  5"] <- 20

#-------------------------------------------------------------------------------------------
#creating extra collumns for:
#LanduseType as defined by the 500m resolution MODIS LandType
#Assigning sites with treecover >10% to forest (This is the definition given by MODIS and the FOA)
#Assigning sites with treecover <10% to non-forest

siteMODIS$LandType500 <- Landuse$types[match(siteMODIS$Landtypepixel, Landuse$MODIS)]
siteMODIS$forestnonforest[siteMODIS$MODISvalue >= 10 & siteMODIS$MODISvalue <= 100] <- "Primary forest"
siteMODIS$forestnonforest[siteMODIS$MODISvalue < 10] <- "Primary non-forest"

#for the NAs assign the value by the LandTypeValue
siteMODIS$forestnonforest[siteMODIS$SSBS == "AD1_2002__Vazquez 1 1 1"] <- "Primary forest"
siteMODIS$forestnonforest[siteMODIS$SSBS == "HW1_2007__Chapman 1 2 37"] <- "Primary non-forest"
siteMODIS$forestnonforest[siteMODIS$SSBS == "JD1_2010__Sodhi 1  5"] <- "Primary forest"
siteMODIS$forestnonforest[siteMODIS$SSBS == "JD1_2002__Pearman 1  4"] <- "Primary non-forest"

View(siteMODIS)
write.csv(siteMODIS, "sites with MODIS info.csv")

forestnonforest <- siteMODIS %>%
  dplyr::select(SSBS, forestnonforest)

write.csv(forestnonforest, "Primary actual land use.csv")






