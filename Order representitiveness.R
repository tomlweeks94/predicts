##Representitivenes figure##
##data required
#names of all the families in PREDICTS (and in general)
#Number of species in PREDICTS
#Number of estimates species described

library(tidyverse)

##read in PREDICTS birds and filter for distinct corrected then find number of families
birds <- readRDS("PREDICTSbirdscorrectnames.rds")
birds$Order_jetz <- jetz$Order[match(birds$Corrected, jetz$Scientific.name)]
order_representitiveness <- birds %>%
  distinct(Corrected, .keep_all = TRUE) %>%
  group_by(Order_jetz) %>%
  mutate(N_Order = n()) %>%
  select(Order_jetz, N_Order) %>%
  distinct(Order, .keep_all = TRUE)



##read in Jetz 2012
jetz <- read.csv("Handbook of the Birds of the World and BirdLife International digital checklist of the birds of the world_Version_4.csv")
jetz <- jetz[!is.na(jetz$ï..Sequence),]
observed_Order <- jetz %>%
  group_by(Order) %>%
  mutate(N_Order_Obs = n()) %>%
  select(Order, N_Order_Obs) %>%
  distinct(Order, .keep_all = TRUE)
colnames(observed_Order) <- c("Order_jetz", "N_Order_Obs")

representitiveness_o <- merge(x = observed_Order, y = order_representitiveness, by = "Order_jetz", all=TRUE)
representitiveness_o$N_Order[is.na(representitiveness_o$N_Order)] <- 0
representitiveness_o$log_Observed <- log10(representitiveness_o$N_Order_Obs)
representitiveness_o$log_PREDICTS <- log10(representitiveness_o$N_Order)


ggplot(representitiveness_o, aes(x=log_Observed, y=log_PREDICTS)) +
  geom_point() +
  xlab("log10 (N estimated described)") +
  ylab("log10 (N representation in PREDICTS database)") + 
  geom_text_repel(aes(label=ifelse(log_PREDICTS < 0, as.character(Order_jetz), "")), force = 25) +
  geom_abline(aes(slope = 1, intercept = -1)) + # the 10% line
  geom_abline(aes(slope = 1, intercept = -2), linetype = "dashed") + # the 1% line
  theme_bw() +
  labs(title = "Represntitiveness of the Bird Orders in the PREDICTS database",
       subtitle = "5 bird orders missing from PREDICTS but all others well represented - Solid line = 10%, Dashed line = 1%")

