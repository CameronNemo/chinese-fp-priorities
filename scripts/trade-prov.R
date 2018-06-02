source("scripts/trade-data.R")
source("scripts/provocations.R")
source("scripts/stats-common.R")

# crossed trade-provocation dataset
trade_prov <-
  wide_growth %>%
  mutate(Year = as.character(Year)) %>%
  left_join(prov_per_year(prov), by=c("Year" = "Year.Start")) %>%
  mutate(Type = gsub("\\s+", "", Type)) %>%
  spread(Type, n)

provocation_vars <- "MilitaryExercise + MissileProvocation + NuclearProvocation + OtherProvocation + Other"
trade_vars <- "JPN.Export + KOR.Export + USA.Export + PRK.Export + JPN.Import + KOR.Import + PRK.Import + USA.Import"

robustify <- function (mod) {
  mod$se <- vcovHC(mod)
}

# models

# strongest tests
mod_kor <- lm(as.formula(paste0("KOR.Export + KOR.Import ~ WLD.Export + WLD.Import + ", provocation_vars)), trade_prov)
mod_jpn <- lm(as.formula(paste0("JPN.Import + JPN.Export ~ WLD.Export + WLD.Import + ", provocation_vars)), trade_prov)
mod_jpn.im <- lm(as.formula(paste0("JPN.Import ~ WLD.Export + WLD.Import + ", provocation_vars)), trade_prov)
mod_prk <- lm(as.formula(paste0("PRK.Export + PRK.Import ~ WLD.Export + WLD.Import + ", provocation_vars)), trade_prov)
mod_prk.ex <- lm(as.formula(paste0("PRK.Export ~ WLD.Export + WLD.Import + ", provocation_vars)), trade_prov)
mod_usa <- lm(as.formula(paste0("USA.Export + USA.Import ~ WLD.Export + WLD.Import + ", provocation_vars)), trade_prov)

mod_kor$se <- vcovHC(mod_kor)
mod_jpn$se <- vcovHC(mod_jpn)
mod_jpn.im$se <- vcovHC(mod_jpn.im)
mod_prk$se <- vcovHC(mod_prk)
mod_prk.ex$se <- vcovHC(mod_prk.ex)
mod_usa$se <- vcovHC(mod_usa)
#lapply(c(mod_prk.ex, mod_jpn.im, mod_jpn), robustify)

mod_tab <- apsrtable(mod_kor,
                     mod_jpn,
                     mod_jpn.im,
                     mod_prk,
                     mod_prk.ex,
                     mod_usa,
                     se=c("pval"),
                     model.names=c("RoK",
                                   "Japan",
                                   "(Imports)",
                                   "DPRK",
                                   "(Exports)",
                                   "USA"),
                     stars="default")
write(mod_tab, "output/models.tex")

# insignificant
mod_prk.im <- lm(as.formula(paste0("PRK.Import ~ WLD.Export + WLD.Import + ", provocation_vars)), trade_prov)
mod_jpn.ex <- lm(as.formula(paste0("JPN.Export ~ WLD.Export + WLD.Import + ", provocation_vars)), trade_prov)
mod_kor.im <- lm(as.formula(paste0("KOR.Import ~ WLD.Export + WLD.Import + ", provocation_vars)), trade_prov)
mod_kor.ex <- lm(as.formula(paste0("KOR.Export ~ WLD.Export + WLD.Import + ", provocation_vars)), trade_prov)
mod_usa.im <- lm(as.formula(paste0("USA.Import ~ WLD.Export + WLD.Import + ", provocation_vars)), trade_prov)
mod_usa.ex <- lm(as.formula(paste0("USA.Export ~ WLD.Export + WLD.Import + ", provocation_vars)), trade_prov)
mod_wld.ex <- lm(as.formula(paste0("WLD.Export ~ ", provocation_vars)), trade_prov)
mod_wld.im <- lm(as.formula(paste0("WLD.Import ~ ", provocation_vars)), trade_prov)
mod_wld <- lm(as.formula(paste0("WLD.Import + WLD.Export ~ ", provocation_vars)), trade_prov)