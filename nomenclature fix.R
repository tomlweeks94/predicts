setwd("C:/Users/tlw119/Dropbox/My drive/PhDizzle/PREDICTS/Birdlife data")

list.files()
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
library(maptools)
library(rgdal)
library(sp)
library(raster)
library(rworldmap)
library(rgeos)



##read in PREDICTS and filter for americas
diversity <- readRDS("PREDICTSbirdscorrectnames.rds") %>%
  filter(Sampling_target == "Entire community")
View(diversity) 

birdcoords <- diversity %>%
  dplyr::select(Corrected, Latitude, Longitude)

setwd("C:/Users/tlw119/Dropbox/My drive/PhDizzle/PREDICTS/Birdlife data")

##Create a crosswalk of all potentials that each entry can be - all splits and lumps
BL1 <- read.csv("BirdLife_Checklist_Version_1.csv")
SL1 <- BL1 %>%
  filter(str_detect(Taxonomic.note, "split|lump")) %>%
  dplyr::select(Scientific.name, Taxonomic.note)
colnames(SL1) <- c("Scientific.name", "Taxonomic.notes")

BL2 <- read.csv("BirdLife_Checklist_Version_2.csv")
SL2 <- BL2 %>%
  filter(str_detect(Taxonomic_note, "split|lump")) %>%
  dplyr::select(Scientific.name, Taxonomic_note)
colnames(SL2) <- c("Scientific.name", "Taxonomic.notes")

BL3 <- read.csv("BirdLife_Checklist_Version_3.csv")
SL3 <- BL3 %>%
  filter(str_detect(Taxonomic_note, "split|lump")) %>%
  dplyr::select(Scientific.name, Taxonomic_note)
colnames(SL3) <- c("Scientific.name", "Taxonomic.notes")

BL4 <- read.csv("BirdLife_Checklist_Version_4.csv")
SL4 <- BL4 %>%
  filter(str_detect(Taxonomic.note, "split|lump")) %>%
  dplyr::select(Scientific.name, Taxonomic.note)
colnames(SL4) <- c("Scientific.name", "Taxonomic.notes")

BL5 <- read.csv("BirdLife_Checklist_Version_5.csv")
SL5 <- BL5 %>%
  filter(str_detect(Taxonomic.note, "split|lump")) %>%
  dplyr::select(Scientific.name, Taxonomic.note)
colnames(SL5) <- c("Scientific.name", "Taxonomic.notes")

BL51 <- read.csv("BirdLife_Checklist_Version_5.1.csv")
SL51 <- BL51 %>%
  filter(str_detect(Taxonomic.note, "split|lump")) %>%
  dplyr::select(Scientific.name, Taxonomic.note)
colnames(SL51) <- c("Scientific.name", "Taxonomic.notes")

BL6 <- read.csv("BirdLife_Checklist_Version_6.csv")
SL6 <- BL6 %>%
  filter(str_detect(Taxonomic.note, "split|lump")) %>%
  dplyr::select(Scientific.name, Taxonomic.note)
colnames(SL6) <- c("Scientific.name", "Taxonomic.notes")

BL61 <- read.csv("BirdLife_Checklist_Version_6.1.csv")
SL61 <- BL61 %>%
  filter(str_detect(Taxonomic.note, "split|lump")) %>%
  dplyr::select(Scientific.name, Taxonomic.note)
colnames(SL61) <- c("Scientific.name", "Taxonomic.notes")

BL7 <- read.csv("BirdLife_Checklist_Version_7.csv")
SL7 <- BL7 %>%
  filter(str_detect(Taxonomic.notes, "split|lump")) %>%
  dplyr::select(Scientific.name, Taxonomic.notes)
colnames(SL7) <- c("Scientific.name", "Taxonomic.notes")

BL8 <- read.csv("BirdLife_Checklist_Version_8.csv")
SL8 <- BL8 %>%
  filter(str_detect(Taxonomic.notes, "split|lump")) %>%
  dplyr::select(Scientific.name, Taxonomic.notes)
colnames(SL8) <- c("Scientific.name", "Taxonomic.notes")

BL9 <- read.csv("BirdLife_Checklist_Version_9.csv")
SL9 <- BL9 %>%
  filter(str_detect(Taxonomic.notes, "split|lump")) %>%
  dplyr::select(Scientific.name, Taxonomic.notes)
colnames(SL9) <- c("Scientific.name", "Taxonomic.notes")

BL91 <- read.csv("BirdLife_Checklist_Version_9.1.csv")
SL91 <- BL91 %>%
  filter(str_detect(Taxonomic.notes, "split|lump")) %>%
  dplyr::select(Scientific.name, Taxonomic.notes)
colnames(SL91) <- c("Scientific.name", "Taxonomic.notes")

HBWBL2 <- read.csv("HBW-BirdLife_Checklist_Version_2.csv")
SLH2 <- HBWBL2 %>%
  filter(str_detect(Taxonomic.notes, "split|lump")) %>%
  dplyr::select(Scientific.name, Taxonomic.notes)
colnames(SLH2) <- c("Scientific.name", "Taxonomic.notes")

HBWBL3 <- read.csv("HBW-BirdLife_Checklist_Version_3.csv")
SLH3 <- HBWBL3 %>%
  filter(str_detect(Taxonomic.notes, "split|lump")) %>%
  dplyr::select(Scientific.name, Taxonomic.notes)
colnames(SLH3) <- c("Scientific.name", "Taxonomic.notes")

HBWBL4 <- read.csv("HBW-BirdLife_Checklist_Version_4.csv")
SLH4 <- HBWBL4 %>%
  filter(str_detect(Taxonomic.notes, "split|lump")) %>%
  dplyr::select(Scientific.name, Taxonomic.notes)
colnames(SLH4) <- c("Scientific.name", "Taxonomic.notes")

SplitsandLumps <- distinct(rbind(SL1,SL2,SL3,SL4,SL5,SL51,SL6,SL61,SL7,SL8,SL9,SL91,SLH2,SLH3,SLH4))
SplitsandLumps <- SplitsandLumps[which(SplitsandLumps$Scientific.name %in% birdcoords$Corrected),]

write.csv(SplitsandLumps, "BL Splits and Lumps in PREDICTS.csv")

#========================
#Here I have manually seperated all the splits identified by birdlife 
#and put potential species names next to the nominate name in PREDICTS
#========================
##fixing duplicates with the birdlife synonym list
#they were highlighted and done manually when table showed >1 for Scientific name

setwd("C:/Users/tlw119/Dropbox/My drive/PhDizzle/PREDICTS/Birdlife data")
synonyms <- read.csv("Birdlife Synonyms.csv")
synonyms <- unique(synonyms)
synonyms <- synonyms[,1:18]
synonyms$ï..Scientific.name <- trimws(synonyms$ï..Scientific.name)
synonyms$synonym.1 <- trimws(synonyms$synonym.1)
synonyms$synonym.2 <- trimws(synonyms$synonym.2)
synonyms$synonym.3 <- trimws(synonyms$synonym.3)
synonyms$synonym.4 <- trimws(synonyms$synonym.4)
synonyms$synonym.5 <- trimws(synonyms$synonym.5)
synonyms$synonym.6 <- trimws(synonyms$synonym.6)
synonyms$synonym.7 <- trimws(synonyms$synonym.7)
synonyms$synonym.8 <- trimws(synonyms$synonym.8)
synonyms$synonym.9 <- trimws(synonyms$synonym.9)
synonyms$synonym.10 <- trimws(synonyms$synonym.10)
synonyms$synonym.11 <- trimws(synonyms$synonym.11)
synonyms$synonym.12 <- trimws(synonyms$synonym.12)
synonyms$synonym.13 <- trimws(synonyms$synonym.13)
synonyms$synonym.14 <- trimws(synonyms$synonym.14)
synonyms$synonym.15 <- trimws(synonyms$synonym.15)
synonyms$synonym.16 <- trimws(synonyms$synonym.16)
synonyms$synonym.17 <- trimws(synonyms$synonym.17)

write.csv(synonyms, "Birdlife Synonyms.csv")

table <- table(synonyms$ï..Scientific.name)
dups <- subset(table, table > 1)

setwd("C:/Users/tlw119/Dropbox/My drive/PhDizzle/PREDICTS")
PBirds<- readRDS("PREDICTSbirdscorrectnames.RDS")
birdoccurence <- PBirds %>%
  dplyr::select(Latitude, Longitude, Corrected)

##merging the different synonyms to the coordinates in PREDICTS
synonymsandcoordinates <- merge(synonyms, birdoccurence, by.x = "ï..Scientific.name", by.y = "Corrected", all.y= TRUE)
write.csv(synonymsandcoordinates, "coordinates and potentials.csv")
##now I will run a script that says "is this coordinate inside of nominate | syn1 |syn2 | syn3......
##This will tell me which species the PREDICTS entry is most likely to be

