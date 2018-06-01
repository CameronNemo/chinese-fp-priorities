source("scripts/trade-data.R")
source("scripts/provocations.R")
source("scripts/stats-common.R")

# crossed trade-provocation dataset
trade_prov <-
  wide_adj_index %>%
  mutate(Year = as.character(Year)) %>%
  left_join(prov_per_year(prov), by=c("Year" = "Year.Start")) %>%
  mutate(Type = gsub("\\s+", "", Type)) %>%
  spread(Type, n)

provocation_vars <- "MilitaryExercise + MissileProvocation + NuclearProvocation + OtherProvocation + Other"
trade_vars <- "JPN.Export + KOR.Export + USA.Export + PRK.Export + JPN.Import + KOR.Import + PRK.Import + USA.Import"

