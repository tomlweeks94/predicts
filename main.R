setwd("C:/Users/tlw119/Dropbox/My drive/PhDizzle/PREDICTS")
##Going through Adriana's online tutorial
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


##read in PREDICTS and filter for americas
diversity <- readRDS("PREDICTSbirdscorrectnames.rds") %>%
  filter(Sampling_target == "Entire community")
View(diversity) 

#Adapt the predominant habitat primarys to reflect the actual forest or non forest as described by MODIS
forestnonforest <- read.csv("Primary actual land use.csv") # CSV created from MODIS code
#diversity$Predominant_habitat <- ifelse(forestnonforest$SSBS == diversity$SSBS, paste(forestnonforest$forestnonforest), paste(diversity$Predominant_habitat))
diversity$forestnonforest <- as.character(forestnonforest$forestnonforest[match(diversity$SSBS, forestnonforest$SSBS)])
diversity$forestnonforest[which(is.na(diversity$forestnonforest))] <- as.character(diversity$Predominant_habitat[which(is.na(diversity$forestnonforest))])
diversity$Predominant_habitat <- diversity$forestnonforest
diversity$Predominant_habitat <- as.factor(diversity$Predominant_habitat)
diversity <- diversity[,1:90]

##Step1 - Create a "LandUse" collumn
diversity <- diversity %>%
  mutate(
    #Creating Land Use collumn - Primary forest --> Primary minimal. All else stays same.
    LandUse = ifelse(grepl("Primary", Predominant_habitat)
                     & Use_intensity == "Minimal use",
                     "Primary minimal",
                     paste(Predominant_habitat)),
    LandUse = ifelse(grepl("secondary", tolower(LandUse)), ##if contains secondary
                     "Secondary vegetation", ## put secondary vegetation
                     paste(LandUse)),
    ##Relevel so that Primary minimal is the first level
    LandUse = factor(LandUse),
    LandUse = relevel(LandUse, ref = "Primary minimal")) %>%
  filter(!is.na(LandUse))

##Separate minimally used primary forest ~ pristine primary vegetation
diversity <- diversity %>%
  mutate(
    #Creating Land Use collumn - Primary forest --> Primary minimal. All else stays same.
    LandUseIntensity = ifelse(LandUse == "Primary minimal", 
                              paste(LandUse), 
                              paste(LandUse, Use_intensity, sep = "-")),
    ##Relevel so that Primary minimal is the first level
    LandUseIntensity = factor(LandUseIntensity),
    LandUseIntensity = relevel(LandUseIntensity, ref = "Primary minimal")
  )


#-----------------------------------------------------------------------------------
#Species diversity at any given site - need to assess if this is what is affecting Functional Diversity#
#-----------------------------------------------------------------------------------

##Step 3
##get the abundance data from the study and create a relative abundance for each species in each site##
diversity <- diversity %>%
  group_by(SSBS) %>% # Group by site
  mutate(SpeciesDiversity = length(unique(Corrected)))
View(diversity)


write_rds(diversity, "PREDICTS_Birds.rds")


#-----------------------------------------------------------------
###Getting the FDs and redundancy for all the sites####
#-----------------------------------------------------------------

##load in the files
GBDavs<- read.delim("GBD_avs.csv", sep = ",")
PBirds<- readRDS("PREDICTSbirdscorrectnames.RDS")

##Check nomenclature is the same which unique species aren't found in the G
PBirdsspecies<- unique(PBirds$Corrected)
mismatched_namesPRED <- PBirdsspecies[which(is.na(match(PBirdsspecies, GBDavs$Unique_Scientific_Name)))]
length(mismatched_namesPRED)

###Remove all except the continuous bird traits and species names for the GBD to create the Species matrix
trait_matrix <- GBDavs[,c(2,109:129)]
trait_matrix <- trait_matrix[,-c(3,5,7,9,11,13,15,17,19,21)]
##make the trait_marix row names the species names (column 1)
row.names(trait_matrix) <- trait_matrix[,1]
##remove column 1
trait_matrix <- trait_matrix[,-1]
trait_matrix <- trait_matrix[,-c(9,11)]
View(trait_matrix)

getFD <- function(s1) {
  
  data_ssbs1 <- filter(diversity, SSBS == s1)
  species1 <- as.character(unique(data_ssbs1$Corrected))
  abundance1 <- data_ssbs1$Measurement[match(species1, data_ssbs1$Corrected)]
  
  com_matrix1 <- rbind(species1, abundance1)
  colnames(com_matrix1) <- com_matrix1[1,]
  com_matrix1 <- t(com_matrix1[-1,])
  rownames(com_matrix1)[1] <- s1
  
  com_matrix1 <- as.data.frame(com_matrix1)
  indx <- sapply(com_matrix1, is.factor)
  com_matrix1[indx] <- lapply(com_matrix1[indx], function(x) as.numeric(as.character(x)))
  
  com_matrix1 <-  com_matrix1[, colSums(com_matrix1 != 0) > 0]
  com_matrix1 <- as.data.frame(com_matrix1)

  if(ncol(com_matrix1) <= 1){ 
    FD <- 0
  } else {
    organize.syncsa(com_matrix1, trait_matrix)
    output<- rao.diversity(com_matrix1, trait_matrix)
    FD<-ifelse(sum(abundance1) == 0, 0, as.numeric(output$FunRao))
  }
  FD
}

getRedundancy <- function(s1) {
  
  data_ssbs1 <- filter(diversity, SSBS == s1)
  species1 <- as.character(unique(data_ssbs1$Corrected))
  abundance1 <- data_ssbs1$Measurement[match(species1, data_ssbs1$Corrected)]
  
  com_matrix1 <- rbind(species1, abundance1)
  colnames(com_matrix1) <- com_matrix1[1,]
  com_matrix1 <- t(com_matrix1[-1,])
  rownames(com_matrix1)[1] <- s1
  
  com_matrix1 <- as.data.frame(com_matrix1)
  indx <- sapply(com_matrix1, is.factor)
  com_matrix1[indx] <- lapply(com_matrix1[indx], function(x) as.numeric(as.character(x)))
  
  com_matrix1 <-  com_matrix1[, colSums(com_matrix1 != 0) > 0]
  com_matrix1 <- as.data.frame(com_matrix1)
  
  if(ncol(com_matrix1) <= 1){ 
    Redundancy <- 0
  } else {
    organize.syncsa(com_matrix1, trait_matrix)
    output<- rao.diversity(com_matrix1, trait_matrix)
    Redundancy<-ifelse(sum(abundance1) == 0, 0, as.numeric(output$FunRedundancy))
  }
  Redundancy
}


sites<- diversity %>%
  group_by(SSBS) %>%
  mutate(speciesatsite = length(unique(Corrected))) %>% 
  ungroup() %>%
  filter(speciesatsite > 1) %>%#Filtering out studies which only look at one species
  group_by(SSBS) %>%
  mutate(measurementsum = sum(Measurement)) %>%
  filter(measurementsum > 0) %>% # filters out sites with no bird abundances
  ungroup() %>%
  distinct(SSBS)

## It is important to recognise that these 0 and NaN sites are still of interest 
## a complete removal of bird assemblages is massive
## only one species should give a functional diversity of 0?

FDs <- vector(mode="numeric", length=length(sites$SSBS))
Redundancy <- vector(mode="numeric", length=length(sites$SSBS))
siteFDs <- data.frame(sites, FDs, Redundancy)


##Looped over sites in multiple parts due to two errors - will be checked lower down
##get the FDs in same way
for (i in 1:length(siteFDs$FDs)) {
  siteFDs$FDs[i] <- getFD(s1 = as.character(siteFDs$SSBS[i]))
}

siteFDs <- rbind(siteFDs1,siteFDs2)

##get the redundancies
for (i in 1:length(siteFDs$Redundancy)) {
  siteFDs$Redundancy[i] <- getRedundancy(s1 = as.character(siteFDs$SSBS[i]))
}


#WRITE THIS siteFDSHIT IT TAKES FUCKING AGES
write.csv(siteFDs, "FDs and Reds.csv")
siteFDs <- read.csv("FDs and Reds.csv")
#####################################


diversity <- readRDS("PREDICTS_Birds.rds")

diversity$FD <- siteFDs$FDs[match(diversity$SSBS, siteFDs$SSBS)]
diversity$FD[which(diversity$FD=="NaN")] <- 0

diversity$Redundancy <- siteFDs$Redundancy[match(diversity$SSBS, siteFDs$SSBS)]
diversity$Redundancy[which(diversity$FD=="NaN")] <- 0

write_rds(diversity, "PREDICTS_Birds_with_FD.rds")


#-----------------------------------------------------------------------------
#FD input data
##This is the FD and Redundancy information which has not been paired or collapsed into only sites
##it is the raw information with every species in every site
#------------------------------------------------------------------------------

diversity <- readRDS("PREDICTS_Birds_with_FD.rds") %>%
  mutate(
    #Paste NAs for can't decide
    LandUse = ifelse(Predominant_habitat == "Cannot decide",
                     NA, ##paste NA
                     paste(LandUse)), ## Leave as LandUse
    LandUse = ifelse(Use_intensity == "Cannot decide",
                     NA, ##paste NA
                     paste(LandUse)), ## Leave as LandUse
    LandUseIntensity = ifelse(LandUseIntensity == "Cropland-Minimal use", paste("Cropland-Light use"), paste(LandUseIntensity)),
    LandUseIntensity = ifelse(LandUse == "Primary forest", paste("Used Primary Forest"), paste(LandUseIntensity)),
    LandUseIntensity = ifelse(LandUse == "Primary non-forest", paste("Used Primary Non-forest"), paste(LandUseIntensity)),
    LandUseIntensity = ifelse(LandUseIntensity == "Pasture-Intense use", paste("Pasture-Higher usage"), paste(LandUseIntensity)),
    LandUseIntensity = ifelse(LandUseIntensity == "Pasture-Light use", paste("Pasture-Higher usage"), paste(LandUseIntensity)),
    LandUseIntensity = ifelse(LandUseIntensity == "Urban-Intense use", paste("Urban-Higher usage"), paste(LandUseIntensity)),
    LandUseIntensity = ifelse(LandUseIntensity == "Urban-Light use", paste("Urban-Higher usage"), paste(LandUseIntensity)),
    LandUseIntensity = ifelse(LandUseIntensity == "Plantation forest-Intense use", paste("Plantation forest-Higher usage"), paste(LandUseIntensity)),
    LandUseIntensity = ifelse(LandUseIntensity == "Plantation forest-Light use", paste("Plantation forest-Higher usage"), paste(LandUseIntensity))
    )


##### Step 4 
##### Creating dataset of entries with info for LU and assesses diversity by abundance,
##### only contains studies with more than one species, some primary minimal data
##### and only one sampling effort
fd_data_input <- diversity %>%
  # drop any rows with unknown LandUse
  filter(!is.na(LandUse)) %>%
  # drop any rows with unknown Landuse
  filter(LandUse != "NA") %>%
  # drop any rows with FD < 0
  filter(FD > 0) %>%
  # pull out only the abundance data
  filter(Diversity_metric_type == "Abundance") %>%
  # group by Study
  group_by(SS) %>%
  # calculate the number of unique sampling efforts within that study
  mutate(n_sample_effort = n_distinct(Sampling_effort)) %>%
  # calculate the number of unique species sampled in that study
  mutate(n_species = n_distinct(Taxon_name_entered)) %>%
  # check if there are any Primary minimal sites in the dataset
  mutate(n_primin_records = sum(LandUse == "Primary minimal")) %>%
  ##create a maximum FD for the FDs within the studies
  mutate(maxFD = max(FD)) %>%
  ##create a maximum redundanct for the Redundancys within the sites
  mutate(maxRedundancy = max(Redundancy)) %>%
  #ungroup
  ungroup() %>%
  ## create a rescaled abundance which 
  mutate(rescaledFD = FD/maxFD) %>%
  ## create a rescaled abundance which 
  mutate(rescaledRedundancy = Redundancy/maxRedundancy) %>%
  # now keep only the studies with one unique sampling effort
  filter(n_sample_effort == 1) %>%
  # and keep only studies with more than one species 
  # as these studies clearly aren't looking at assemblage-level diversity
  filter(n_species > 1) %>%
  # and keep only studies with at least some Primary minimal data
  #filter(n_primin_records > 0) %>% # This pulls out all potential those which aren't comparing to a pristine site
  # drop empty factor levels
  droplevels()
View(fd_data_input)

write.csv(fd_data_input, "fd_data_input.csv")

#--------------------------------------------------------------------------------------
#Creating the per site FD and Redundancy (non pairwise)
#Pulling out only the distinct sites - keeping only redundancy and FD information
#--------------------------------------------------------------------------------------
fd_data_unpaired <- fd_data_input %>%
  distinct(SSBS, .keep_all = TRUE) %>%
  dplyr::select(SSBS, 
         SS, 
         SSB,
         SpeciesDiversity,
         FD,
         rescaledFD,
         Redundancy,
         rescaledRedundancy,
         LandUse,
         LandUseIntensity)

write.csv(fd_data_unpaired, "rescaled_FD_Redundancy_at_each_site.csv")
View(fd_data_unpaired)



