#-----------------------------------------------------------------------------------------------------------------
###Crearing total abundance at each site (perhaps competition plays a part in the effects on functional diversity)
#-----------------------------------------------------------------------------------------------------------------
##Step 2 - Calculating the total abundance at each site
total_community_abundance_data <- diversity %>%
  # pull out just the abundance measures
  filter(Diversity_metric_type == "Abundance") %>%
  # group by SSBS (each unique value corresponds to a unique site)
  group_by(SSBS) %>%
  # now add up all the abundance measurements within each site
  mutate(TotalAbundance = sum(Measurement)) %>%
  # ungroup
  ungroup() %>%
  # pull out unique sites
  distinct(SSBS, .keep_all = TRUE) %>%
  # now group by Study ID
  group_by(SS) %>%
  # pull out the maximum abundance for each study
  mutate(MaxAbundance = max(TotalAbundance)) %>%
  # ungroup
  ungroup() %>%
  # now rescale total abundance, so that within each study, abundance varies from 0 to 1.
  mutate(RescaledAbundance = TotalAbundance/MaxAbundance)
View(total_community_abundance_data)