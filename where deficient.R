###Creating a df to see where we have gaps for birds
setwd("C:/Users/tomlw/Desktop/PhDizzle/PREDICTS")
PREDICTSbirds<- readRDS("PREDICTSbirdscorrectnames.rds")
View(PREDICTSbirds)

birdsbyEcoR <- table(PREDICTSbirds$Ecoregion)

birdsbyBiome <- table(PREDICTSbirds$Biome)
##Deficient in Flooded Grasslands & Savannas, Inland Water, Rock & Ice, Mangroves(72)
birdsbyRealm <- table(PREDICTSbirds$Realm)
##Deficient in Antartica 
birdsbyHabitat <- table(PREDICTSbirds$Predominant_habitat)
##No deficiencies

##GLOBAL SHOULD BE FINE##
