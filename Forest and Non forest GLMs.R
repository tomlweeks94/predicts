fd_data_input <- read.csv("fd_data_input.csv")
#================================================
#Bellow is using only studies where the comparison is to Primary Forest
#================================================
#forest_data_unpaired <- fd_data_input %>%
  #group_by(SS) %>%
  #mutate(n_forest_habitats = length(which(Predominant_habitat == "Primary forest"))) %>%
  #ungroup() %>%
  #filter(n_forest_habitats > 0) %>%
  #filter(Predominant_habitat != "Primary non-forest") %>%
  #distinct(SSBS, .keep_all = TRUE) %>%
  #dplyr::select(SSBS, 
                #SS, 
                #SSB,
                #SpeciesDiversity,
                #FD,
                #rescaledFD,
                #Redundancy,
                #rescaledRedundancy,
                #LandUse,
                #LandUseIntensity)

#============================
#Bellow is the one Im currently using: This is just removed all primary non-forests so Primary minimal is now just PFMU'
#============================

forest_data_unpaired <- fd_data_input %>%
  filter(Predominant_habitat != "Primary non-forest") %>%
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


##relevel
forest_data_unpaired$LandUse = factor(forest_data_unpaired$LandUse)
forest_data_unpaired$LandUse = relevel(forest_data_unpaired$LandUse, ref = "Primary minimal") ## Leave as LandUse
forest_data_unpaired$LandUseIntensity = factor(forest_data_unpaired$LandUseIntensity)
forest_data_unpaired$LandUseIntensity = relevel(forest_data_unpaired$LandUseIntensity, ref = "Primary minimal")


##--------------------------------------
###FD
##--------------------------------------

errorbar <- forest_data_unpaired %>%
  group_by(LandUse) %>%
  summarise(mn = mean((1/(1-FD))),
            se = (sqrt(var((1/(1-FD
                               )))))/sqrt(nrow(forest_data_unpaired))) 

ggplot(forest_data_unpaired, aes(x= LandUse, y=(1/(1-FD)))) +
  geom_point(alpha = 0.1, colour = "black") +
  geom_point(data = errorbar, aes(y=mn, colour = LandUse), size = 5) +
  scale_color_manual(values = c("Primary non-forest" = "black", "Urban" = "black", "Secondary vegetation" = "green3", "Plantation forest" = "green3", "Cropland" = "green3", "Pasture" = "black", "Primary forest" = "green3", "Primary minimal" = "red")) +
  #geom_errorbar(data = errorbar, aes(x = LandUse, ymin = mn-se, ymax=mn+se), size = 1, width = 0.2, col = "red", inherit.aes = FALSE) +
  geom_abline(aes(slope = 0, intercept = as.numeric(forest_data_unpaired %>%
                                                      filter(LandUse == "Primary minimal") %>%
                                                      summarise(mean((1/(1-FD)))))), colour = "blue", linetype = "dashed", size = 1) +
  coord_flip()

modrescaled <- lmer((1/(1-FD)) ~ SpeciesDiversity + LandUse + (1|SS) + (1|SSB), data = forest_data_unpaired)
summary(modrescaled)

qqnorm(resid(modrescaled))
qqline(resid(modrescaled))

##----------------
#REDUNDANCY
##-----------------

errorbar2 <- forest_data_unpaired %>%
  group_by(LandUse) %>%
  summarise(mn = mean((1/(1-Redundancy))),
            se = (sqrt(var((1/(1-Redundancy)))))/sqrt(nrow(forest_data_unpaired))) 


ggplot(forest_data_unpaired, aes(x= LandUse, y=(1/(1-Redundancy)))) +
  geom_point(alpha = 0.1, colour = "black") +
  geom_point(data = errorbar2, aes(y=mn, colour = LandUse), size = 5) +
  scale_color_manual(values = c("Urban" = "black", "Secondary vegetation" = "black", "Plantation forest" = "green3", "Cropland" = "green3", "Pasture" = "green3", "Primary forest" = "green3", "Primary minimal" = "red")) +
  #geom_errorbar(data = errorbar2, aes(x = LandUse, ymin = mn-se, ymax=mn+se), size = 1, width = 0.2, col = "red", inherit.aes = FALSE) +
  geom_abline(aes(slope = 0, intercept = as.numeric(forest_data_unpaired %>%
                                                      filter(LandUse == "Primary minimal") %>%
                                                      summarise(mean((1/(1-Redundancy)))))), colour = "blue", linetype = "dashed", size = 1) +
  coord_flip()



modrescaledRED <- lmer((1/(1-Redundancy)) ~ SpeciesDiversity + LandUse + (1|SS) + (1|SSB), data = forest_data_unpaired)
summary(modrescaledRED)

#==============================================================================================================================
#non-forest
#==============================================================================================================================
#Bellow is using only studies where the comparison is to Primary Forest
#================================================

#nonforest_data_unpaired <- fd_data_input %>%
  #group_by(SS) %>%
  #mutate(n_nonforest_habitats = length(which(Predominant_habitat == "Primary non-forest"))) %>%
  #ungroup() %>%
  #filter(n_nonforest_habitats > 0) %>%
  #filter(Predominant_habitat != "Primary forest") %>%
  #distinct(SSBS, .keep_all = TRUE) %>%
  #dplyr::select(SSBS, 
                #SS, 
                #SSB,
                #SpeciesDiversity,
                #FD,
                #rescaledFD,
                #Redundancy,
                #rescaledRedundancy,
                #LandUse,
                #LandUseIntensity)

#=============================
#Bellow is the one I am using which just removes any Primary forests so Primary minimal is just PNFMU
#-----------------------------

nonforest_data_unpaired <- fd_data_input %>%
  filter(Predominant_habitat != "Primary forest") %>%
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

##relevel
nonforest_data_unpaired$LandUse = factor(nonforest_data_unpaired$LandUse)
nonforest_data_unpaired$LandUse = relevel(nonforest_data_unpaired$LandUse, ref = "Primary minimal") ## Leave as LandUse
nonforest_data_unpaired$LandUseIntensity = factor(nonforest_data_unpaired$LandUseIntensity)
nonforest_data_unpaired$LandUseIntensity = relevel(nonforest_data_unpaired$LandUseIntensity, ref = "Primary minimal")


##--------------------------------------
###FD
##--------------------------------------

errorbar <- nonforest_data_unpaired %>%
  group_by(LandUse) %>%
  summarise(mn = mean((1/(1-FD))),
            se = (sqrt(var((1/(1-FD
            )))))/sqrt(nrow(nonforest_data_unpaired))) 

ggplot(nonforest_data_unpaired, aes(x= LandUse, y=(1/(1-FD)))) +
  geom_point(alpha = 0.1, colour = "black") +
  geom_point(data = errorbar, aes(y=mn, colour = LandUse), size = 5) +
  scale_color_manual(values = c("Primary non-forest" = "black", "Urban" = "black", "Secondary vegetation" = "black", "Plantation forest" = "black", "Cropland" = "black", "Pasture" = "black", "Primary forest" = "black", "Primary minimal" = "red")) +
  #geom_errorbar(data = errorbar, aes(x = LandUse, ymin = mn-se, ymax=mn+se), size = 1, width = 0.2, col = "red", inherit.aes = FALSE) +
  geom_abline(aes(slope = 0, intercept = as.numeric(nonforest_data_unpaired %>%
                                                      filter(LandUse == "Primary minimal") %>%
                                                      summarise(mean((1/(1-FD)))))), colour = "blue", linetype = "dashed", size = 1) +
  coord_flip()

modrescaled <- lmer((1/(1-FD)) ~ SpeciesDiversity + LandUse + (1|SS) + (1|SSB), data = nonforest_data_unpaired)
summary(modrescaled)

qqnorm(resid(modrescaled))
qqline(resid(modrescaled))

##----------------
#REDUNDANCY
##-----------------

errorbar2 <- nonforest_data_unpaired %>%
  group_by(LandUse) %>%
  summarise(mn = mean((1/(1-Redundancy))),
            se = (sqrt(var((1/(1-Redundancy)))))/sqrt(nrow(nonforest_data_unpaired))) 


ggplot(nonforest_data_unpaired, aes(x= LandUse, y=(1/(1-Redundancy)))) +
  geom_point(alpha = 0.1, colour = "black") +
  geom_point(data = errorbar2, aes(y=mn, colour = LandUse), size = 5) +
  scale_color_manual(values = c("Primary non-forest" = "black", "Urban" = "black", "Secondary vegetation" = "black", "Plantation forest" = "black", "Cropland" = "black", "Pasture" = "black", "Primary forest" = "black", "Primary minimal" = "red")) +
  #geom_errorbar(data = errorbar2, aes(x = LandUse, ymin = mn-se, ymax=mn+se), size = 1, width = 0.2, col = "red", inherit.aes = FALSE) +
  geom_abline(aes(slope = 0, intercept = as.numeric(nonforest_data_unpaired %>%
                                                      filter(LandUse == "Primary minimal") %>%
                                                      summarise(mean((1/(1-Redundancy)))))), colour = "blue", linetype = "dashed", size = 1) +
  coord_flip()



modrescaledRED <- lmer((1/(1-Redundancy)) ~ SpeciesDiversity + LandUse + (1|SS) + (1|SSB), data = nonforest_data_unpaired)
summary(modrescaledRED)



