setwd("C:/Users/tlw119/Dropbox/My drive/PhDizzle/PREDICTS")
##Load in the libraries
library(dplyr) # for easy data manipulation
library(stringr)
library(stringi)
library(tidyr) # ditto
library(magrittr) # for piping
library(lme4) # for mixed effects models
library(lmerTest)
library(car) # for logit transformation with adjustment
library(foreach) # running loops
library(doParallel) # running loops in parallel
library(raster) # for working with raster data
library(geosphere) # calculating geographic distance between sites
library(SYNCSA)
library(ggplot2)
library(tidyverse)
fd_data_unpaired <- read.csv("rescaled_FD_Redundancy_at_each_site.csv")
##relevel
fd_data_unpaired$LandUse = factor(fd_data_unpaired$LandUse)
fd_data_unpaired$LandUse = relevel(fd_data_unpaired$LandUse, ref = "Primary minimal") ## Leave as LandUse
fd_data_unpaired$LandUseIntensity = factor(fd_data_unpaired$LandUseIntensity)
fd_data_unpaired$LandUseIntensity = relevel(fd_data_unpaired$LandUseIntensity, ref = "Primary minimal")


##--------------------------------------
###FD
##--------------------------------------

errorbar <- fd_data_unpaired %>%
  group_by(LandUse) %>%
  summarise(mn = mean((1/(1-FD))),
            se = (sqrt(var((1/(1-FD)))))/sqrt(nrow(fd_data_unpaired))) 

ggplot(fd_data_unpaired, aes(x= LandUse, y=(1/(1-FD)))) +
  geom_point(alpha = 0.1, colour = "black") +
  geom_point(data = errorbar, aes(y=mn, colour = LandUse), size = 5) +
  scale_color_manual(values = c("Primary non-forest" = "black", "Urban" = "black", "Secondary vegetation" = "green3", "Plantation forest" = "green3", "Cropland" = "green3", "Pasture" = "black", "Primary forest" = "green3", "Primary minimal" = "red")) +
  #geom_errorbar(data = errorbar, aes(x = LandUse, ymin = mn-se, ymax=mn+se), size = 1, width = 0.2, col = "red", inherit.aes = FALSE) +
  geom_abline(aes(slope = 0, intercept = as.numeric(fd_data_unpaired %>%
                                                      filter(LandUse == "Primary minimal") %>%
                                                      summarise(mean((1/(1-FD)))))), colour = "blue", linetype = "dashed", size = 1) +
  coord_flip()

modrescaled <- lmer((1/(1-FD)) ~ SpeciesDiversity + LandUse + (1|SS) + (1|SSB), data = fd_data_unpaired)
summary(modrescaled)

qqnorm(resid(modrescaled))
qqline(resid(modrescaled))

##----------------
#REDUNDANCY
##-----------------

errorbar2 <- fd_data_unpaired %>%
  group_by(LandUse) %>%
  summarise(mn = mean((1/(1-Redundancy))),
            se = (sqrt(var((1/(1-Redundancy)))))/sqrt(nrow(fd_data_unpaired))) 


ggplot(fd_data_unpaired, aes(x= LandUse, y=(1/(1-Redundancy)))) +
  geom_point(alpha = 0.1, colour = "black") +
  geom_point(data = errorbar2, aes(y=mn, colour = LandUse), size = 5) +
  scale_color_manual(values = c("Primary non-forest" = "black", "Urban" = "black", "Secondary vegetation" = "green3", "Plantation forest" = "green3", "Cropland" = "green3", "Pasture" = "green3", "Primary forest" = "green3", "Primary minimal" = "red")) +
  #geom_errorbar(data = errorbar2, aes(x = LandUse, ymin = mn-se, ymax=mn+se), size = 1, width = 0.2, col = "red", inherit.aes = FALSE) +
  geom_abline(aes(slope = 0, intercept = as.numeric(fd_data_unpaired %>%
                                                      filter(LandUse == "Primary minimal") %>%
                                                      summarise(mean((1/(1-Redundancy)))))), colour = "blue", linetype = "dashed", size = 1) +
  coord_flip()



modrescaledRED <- lmer((1/(1-Redundancy)) ~ SpeciesDiversity + LandUse + (1|SS) + (1|SSB), data = fd_data_unpaired)
summary(modrescaledRED)



#-------------------------------------------
#With intensity
#-------------------------------------------
errorbar3 <- fd_data_unpaired %>%
  group_by(LandUseIntensity) %>%
  summarise(mn = mean((1/(1-FD))),
            se = (sqrt(var((1/(1-FD)))))/sqrt(nrow(fd_data_unpaired))) 


ggplot(fd_data_unpaired, aes(x= LandUseIntensity, y=(1/(1-FD)))) +
  geom_point(alpha = 0.1, colour = "black") +
  geom_point(data = errorbar3, aes(y=mn, colour = LandUseIntensity), size = 5) +
  scale_color_manual(values = c("Used Primary Non-forest" = "black", "Urban-Minimal use" = "black", "Urban-Higher usage" = "black", "Secondary vegetation-Light use" = "green3", "Secondary vegetation-Intense use" = "black", "Plantation forest-Minimal use" = "black", "Cropland-Light use" = "black", "Pasture-Higher usage" = "black", "Cropland-Intense use"= "green3", "Pasture-Minimal use"= "black", "Plantation forest-Higher usage"= "green3", "Secondary vegetation-Minimal use"= "black", "Used Primary Forest" = "green3", "Primary minimal" = "red")) +
  #geom_errorbar(data = errorbar3, aes(x = LandUseIntensity, ymin = mn-se, ymax=mn+se), size = 1, width = 0.2, col = "red", inherit.aes = FALSE) +
  geom_abline(aes(slope = 0, intercept = as.numeric(fd_data_unpaired %>%
                                                      filter(LandUseIntensity == "Primary minimal") %>%
                                                      summarise(mean((1/(1-FD)))))), colour = "blue", linetype = "dashed", size = 1) +
  coord_flip()


modrescaled <- lmer((1/(1-FD)) ~ SpeciesDiversity + LandUseIntensity + (1|SS) + (1|SSB), data = fd_data_unpaired)
summary(modrescaled)

#qqnorm(resid(modrescaled))
#qqline(resid(modrescaled))

##----------------
#REDUNDANCY
##-----------------

errorbar4 <- fd_data_unpaired %>%
  group_by(LandUseIntensity) %>%
  summarise(mn = mean((1/(1-Redundancy))),
            se = (sqrt(var((1/(1-Redundancy)))))/sqrt(nrow(fd_data_unpaired))) 


ggplot(fd_data_unpaired, aes(x= LandUseIntensity, y=(1/(1-Redundancy)))) +
  geom_point(alpha = 0.1) +
  geom_point(data = errorbar4, aes(y=mn, colour = LandUseIntensity), size = 5) +
  scale_color_manual(values = c("Used Primary Non-forest" = "black", "Urban-Minimal use" = "black", "Urban-Higher usage" = "black", "Secondary vegetation-Light use" = "black", "Secondary vegetation-Intense use" = "black", "Plantation forest-Minimal use" = "black", "Cropland-Light use" = "black", "Pasture-Higher usage" = "black", "Cropland-Intense use"= "green3", "Pasture-Minimal use"= "green3", "Plantation forest-Higher usage"= "green3", "Secondary vegetation-Minimal use"= "green3", "Used Primary Forest" = "green3", "Primary minimal" = "red")) +
  #geom_errorbar(data = errorbar4, aes(x = LandUseIntensity, ymin = mn-se, ymax=mn+se), size = 1, width = 0.2, col = "red", inherit.aes = FALSE) +
  geom_abline(aes(slope = 0, intercept = as.numeric(fd_data_unpaired %>%
                                                      filter(LandUseIntensity == "Primary minimal") %>%
                                                      summarise(mean((1/(1-Redundancy)))))), colour = "blue", linetype = "dashed", size = 1) +
  coord_flip()


modrescaledRED <- lmer((1/(1-Redundancy)) ~ SpeciesDiversity + LandUseIntensity + (1|SS) + (1|SSB), data = fd_data_unpaired)
summary(modrescaledRED)










