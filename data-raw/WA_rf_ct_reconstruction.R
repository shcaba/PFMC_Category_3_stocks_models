library(tidyverse)

#Read in the WA rockfish landings reconstruction.
#This takes a total pounds of landings in a year, then uses a percent per species to break it out by species
WA_rf_ct_recon <- read.csv(
  "G:/Shared drives/NMFS NWC FRAM Population Ecology/Assessment Data (contains PII)/2027 Assessment Cycle/Category_3/WA rockfish catch reconstruction/SpeciesSumOutput2_2017.csv"
)

#Indentify the category 3 stocks to pull the landings
SPID_cat3_rf <- c("BANK", "RSTN", "SRKR", "STAR", "STRK", "SLGR", "YMTH")

#Remove the values that are already in PacFIN. Ultimately the values pulled here need to be combine with PacFIN, as PacFIN does not have the URCK or UPOP categories broken out to species, just the species-specific landings.
WA_rf_ct_recon_nopacfin <- subset(
  WA_rf_ct_recon,
  CompositionType != "Existing PacFIN"
)

#Change pounds to MT
WA_rf_ct_recon_nopacfin$MT <- WA_rf_ct_recon_nopacfin$SpeciesPounds / 2204.62
#
Cat3.rf.recon.landings.wa <- WA_rf_ct_recon_nopacfin %>%
  select(Year, SPID, MT) %>%
  filter(SPID %in% SPID_cat3_rf) %>%
  group_by(Year, SPID) %>%
  summarize(MT = sum(MT, na.rm = TRUE)) %>%
  pivot_wider(names_from = SPID, values_from = MT, values_fill = 0)
