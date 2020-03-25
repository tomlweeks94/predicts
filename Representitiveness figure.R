##Representitivenes figure##
##data required
#names of all the families in PREDICTS (and in general)
#Number of species in PREDICTS
#Number of estimates species described

library(tidyverse)

##read in PREDICTS birds and filter for distinct corrected then find number of families
birds <- readRDS("PREDICTS_Birds_with_FD.rds")
birds$Family <- replace(birds$Family, birds$Family == "Pandionidae", "Accipitridae")
fam_representitiveness <- birds %>%
  distinct(Corrected, .keep_all = TRUE) %>%
  group_by(Family) %>%
  mutate(N_fam = n()) %>%
  select(Family, N_fam) %>%
  distinct(Family, .keep_all = TRUE)

##read in Jetz 2012
jetz <- read.csv("Copy of BirdLife_Checklist_Version_3.csv")
observed <- jetz %>%
  group_by(ï..Family.name) %>%
  mutate(N_Fam_Obs = n()) %>%
  select(ï..Family.name, N_Fam_Obs) %>%
  distinct(ï..Family.name, .keep_all = TRUE)
observed <- observed[1:197,]
colnames(observed) <- c("Family", "Jetz")

observed$Family <- as.character(observed$Family)
observed$Family <- replace(observed$Family, observed$Family =="Reguliidae", values = "Regulidae")

representitiveness <- merge(x = observed, y = fam_representitiveness, by = "Family", all=TRUE)
representitiveness$N_fam[is.na(representitiveness$N_fam)] <- 0
representitiveness <- na.omit(representitiveness)
representitiveness$log_Observed <- log10(representitiveness$Jetz)
representitiveness$log_Observed
representitiveness$log_PREDICTS <- log10(representitiveness$N_fam)
representitiveness <- subset(representitiveness, representitiveness$log_PREDICTS >= 0)

ggplot(representitiveness, aes(x=log_Observed, y=log_PREDICTS)) +
  geom_point(alpha = 0.5) +
  xlab("log10 (N estimated described)") +
  ylab("log10 (N representation in PREDICTS database)") +
  geom_abline(aes(slope = 1, intercept = -1)) + # the 10% line
  geom_abline(aes(slope = 1, intercept = -2), linetype = "dashed") + # the 1% line
  theme_bw() +
  labs(title = "Represntitiveness of the Bird Families in the PREDICTS database (71% total bird Families)",
       subtitle = "70 families missing from PREDICTS but all others well represented - Solid line = 10%, Dashed line = 1%")

  
  labs(title = labs(title = "Represntitiveness of the Bird Families in the PREDICTS database (71% total bird Families)",
                    subtitle = "70 families missing from PREDICTS but all others well represented - Solid line = 10%, Dashed line = 1%"))

  missingfromPREDICTS <- representitiveness$Family[which(is.na(representitiveness$N_fam))]
  
  
  
  
  
  
  
  
  
  
  





ggplot(representitiveness, aes(x=Jetz, y=N_fam)) +
  geom_point() +
  geom_abline(aes(slope = 1/10, intercept = 0)) + 
  geom_abline(aes(slope = 1/100, intercept = 0)) +
  geom_abline(aes(slope = 1/1000, intercept = 0))

##Slope is the change in log(Y) when the log(X) changes by 1.0.


View(representitiveness)

