source("scripts/trade-data.R")
source("scripts/provocations.R")
source("scripts/stats-common.R")

# crossed trade-provocation dataset
trade_prov <-
  chn_prk_spread %>%
  left_join(prov_per_year(prov), by="Year.Start") %>%
  mutate(Type = gsub("\\s+", "", Type)) %>%
  spread(Type, n)

ex_prov_mod <- lm(Export ~ MilitaryExercise + MissileProvocation + NuclearProvocation + OtherProvocation + Other, trade_prov)
im_prov_mod <- lm(Import ~ MilitaryExercise + MissileProvocation + NuclearProvocation + OtherProvocation + Other, trade_prov)