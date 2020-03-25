###ggplot and GLM for rescaled - seems unlikely now

##--------------------------------------
###FD
##--------------------------------------

errorbar <- fd_data_unpaired %>%
  group_by(LandUse) %>%
  summarise(mn = mean((rescaledFD^2)),
            se = (sqrt(var((rescaledFD^2))))/sqrt(nrow(fd_data_unpaired))) 

library(ggplot2)
ggplot(fd_data_unpaired, aes(x= LandUse, y=(rescaledFD^2))) +
  geom_point(alpha = 0.1, colour = "black") +
  geom_point(data = errorbar, aes(y=mn), size = 5, col = "red") +
  geom_errorbar(data = errorbar, aes(x = LandUse, ymin = mn-se, ymax=mn+se), size = 1, width = 0.2, col = "red", inherit.aes = FALSE) +
  geom_abline(aes(slope = 0, intercept = as.numeric(fd_data_unpaired %>%
                                                      filter(LandUse == "Primary minimal") %>%
                                                      summarise(mean((rescaledFD^2))))), colour = "blue", linetype = "dashed", size = 1) +
  coord_flip()

library(lmerTest)
modrescaled <- lmer((rescaledFD^2) ~ LandUse + (1|SS) + (1|SSB), data = fd_data_unpaired)
summary(modrescaled)

qqnorm(resid(modrescaled))
qqline(resid(modrescaled))

##----------------
#REDUNDANCY
##-----------------

errorbar2 <- fd_data_unpaired %>%
  group_by(LandUse) %>%
  summarise(mn = mean(rescaledRedundancy),
            se = (sqrt(var(rescaledRedundancy)))/sqrt(nrow(fd_data_unpaired))) 

library(ggplot2)
ggplot(fd_data_unpaired, aes(x= LandUse, y=rescaledRedundancy)) +
  geom_point(alpha = 0.1, colour = "black") +
  geom_point(data = errorbar2, aes(y=mn), size = 5, col = "red") +
  geom_errorbar(data = errorbar2, aes(x = LandUse, ymin = mn-se, ymax=mn+se), size = 1, width = 0.2, col = "red", inherit.aes = FALSE) +
  geom_abline(aes(slope = 0, intercept = as.numeric(fd_data_unpaired %>%
                                                      filter(LandUse == "Primary minimal") %>%
                                                      summarise(mean(rescaledRedundancy)))), colour = "blue", linetype = "dashed", size = 1) +
  coord_flip()


library(lmerTest)
modrescaledRED <- lmer(rescaledRedundancy ~ LandUse + (1|SS) + (1|SSB), data = fd_data_unpaired)
summary(modrescaledRED)



#-------------------------------------------
#With intensity
#-------------------------------------------
errorbar3 <- fd_data_unpaired %>%
  group_by(LandUseIntensity) %>%
  summarise(mn = mean((rescaledFD^2)),
            se = (sqrt(var((rescaledFD^2))))/sqrt(nrow(fd_data_unpaired))) 

library(ggplot2)
ggplot(fd_data_unpaired, aes(x= LandUseIntensity, y=(rescaledFD^2))) +
  geom_point(alpha = 0.1, colour = "black") +
  geom_point(data = errorbar3, aes(y=mn), size = 5, col = "red") +
  geom_errorbar(data = errorbar3, aes(x = LandUseIntensity, ymin = mn-se, ymax=mn+se), size = 1, width = 0.2, col = "red", inherit.aes = FALSE) +
  geom_abline(aes(slope = 0, intercept = as.numeric(fd_data_unpaired %>%
                                                      filter(LandUseIntensity == "Primary minimal") %>%
                                                      summarise(mean((rescaledFD^2))))), colour = "blue", linetype = "dashed", size = 1) +
  coord_flip()

library(lmerTest)
modrescaled <- lmer((rescaledFD^2) ~ LandUseIntensity + (1|SS) + (1|SSB), data = fd_data_unpaired)
summary(modrescaled)

qqnorm(resid(modrescaled))
qqline(resid(modrescaled))

##----------------
#REDUNDANCY
##-----------------

errorbar4 <- fd_data_unpaired %>%
  group_by(LandUseIntensity) %>%
  summarise(mn = mean(rescaledRedundancy),
            se = (sqrt(var(rescaledRedundancy)))/sqrt(nrow(fd_data_unpaired))) 

library(ggplot2)
ggplot(fd_data_unpaired, aes(x= LandUseIntensity, y=rescaledRedundancy)) +
  geom_point(alpha = 0.1, colour = "black") +
  geom_point(data = errorbar4, aes(y=mn), size = 5, col = "red") +
  geom_errorbar(data = errorbar4, aes(x = LandUseIntensity, ymin = mn-se, ymax=mn+se), size = 1, width = 0.2, col = "red", inherit.aes = FALSE) +
  geom_abline(aes(slope = 0, intercept = as.numeric(fd_data_unpaired %>%
                                                      filter(LandUseIntensity == "Primary minimal") %>%
                                                      summarise(mean(rescaledRedundancy)))), colour = "blue", linetype = "dashed", size = 1) +
  coord_flip()


library(lmerTest)
modrescaledRED <- lmer(rescaledRedundancy ~ LandUseIntensity + (1|SS) + (1|SSB), data = fd_data_unpaired)
summary(modrescaledRED)










