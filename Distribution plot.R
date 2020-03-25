##Plot of bird distribution with variable size of point describing number of birds from that site
##load correct libraries
library(ggplot2)
library(dplyr)
library(maps)
corrected <- readRDS("PREDICTSbirdscorrectnames.rds") %>%
  distinct(SSBS, .keep_all = TRUE)
corrected$coords <- paste(corrected$Longitude,corrected$Latitude)
corrected$number_sites <- NA
corrected$number_sites <- as.numeric(corrected$number_sites)
##create a vector which has the numbers of times an entry is from the same location##

for(i in 1:length(corrected$number_sites)) {
  corrected$number_sites[i] <- nrow(corrected[corrected$coords == corrected$coords[i],])
}

##download map data for the world
world<- map_data("world")
# virids package for the color palette
library(viridis)
# create plot with all PREDICTS coordinates with size and colour depicting the amount of info from each site
ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=corrected, aes(x=Longitude, y=Latitude, size=number_sites, color=number_sites)) +
  scale_size_continuous(range=c(1,12))+
  scale_color_viridis(trans="log") +
  theme_bw() +
  labs(title = "Global distribution of PREDICTS sites with bird assemblages")
