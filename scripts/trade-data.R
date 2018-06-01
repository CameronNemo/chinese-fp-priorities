source("scripts/common.R")

comtrade_files <- c("data/comtrade0307.csv",
                    "data/comtrade0812.csv",
                    "data/comtrade1317.csv",
                    "data/comtrade9802.csv",
                    "data/comtrade9397.csv")

clean_comtrade <- function(file_name) {
  read.csv(file_name) %>%
    mutate(Trade.Value = Trade.Value..US..) %>%
    select(Year, Reporter, Reporter.ISO, Partner, Partner.ISO, Trade.Flow, Trade.Value)
}

long <- do.call("rbind", lapply(comtrade_files, clean_comtrade))

# filter for China's reported trade
long %<>%
  filter(Reporter.ISO == "CHN") %>%
  select(Partner.ISO, Year, Trade.Flow, Trade.Value)

# subtract re-imports
wide_adj <-
  long %>%
  mutate(Partner.Flow = paste(Partner.ISO, Trade.Flow, sep=".")) %>%
  select(Year, Partner.Flow, Trade.Value) %>%
  spread(Partner.Flow, Trade.Value) %>%
  mutate(WLD.Import = WLD.Import - ifelse(is.na(`WLD.Re-Import`), yes=0, no=`WLD.Re-Import`)) %>%
  select(-`WLD.Re-Import`)

# add Trade.Index variable
long_adj_index <-
  wide_adj %>%
  gather(Partner.Flow, Trade.Value, -Year) %>%
  separate(Partner.Flow, c('Partner.ISO', 'Trade.Flow'), sep="\\.") %>%
  group_by(Partner.ISO, Trade.Flow) %>%
  mutate(Trade.Index = ((Trade.Value / Trade.Value[Year == min(Year)]) - 1) * 100) %>%
  ungroup()

# add Trade.Growth variable
long_growth <-
  long_adj_index %>%
  mutate(Partner.Flow = paste(Partner.ISO, Trade.Flow, sep=".")) %>%
  select(Year, Partner.Flow, Trade.Value) %>%
  group_by(Partner.Flow) %>%
  mutate(Trade.ValueLag = lag(Trade.Value, 1),
         Trade.Growth = ifelse(is.na(Trade.ValueLag), NA,
                               (Trade.Value - Trade.ValueLag) / Trade.ValueLag)) %>%
  select(-Trade.ValueLag, -Trade.Value) %>%
  ungroup() %>%
  # filter NA values
  filter(Year > min(Year))

# widen to make regressions easier
wide_adj_index <-
  long_adj_index %>%
  mutate(Partner.Flow = paste(Partner.ISO, Trade.Flow, sep=".")) %>%
  select(Year, Partner.Flow, Trade.Index) %>%
  spread(Partner.Flow, Trade.Index)

wide_growth  <-
  long_growth %>%
  spread(Partner.Flow, Trade.Growth)

# TODO: 2017supplement, check portions

# portions
#part_year$trade_portion <- part_year$trade_usd / part_year$trade_usd[part_year$Partner == "World"]
# alternative to spread()
#data.table::dcast(setDT(dd), Year ~ Trade.Flow, value.var = names(dd)[3:7])

# adjust PRK trade index for global trends
#chn_prk_adj <-
#  part_year_chn %>%
#  filter(Partner.ISO == "PRK")
#chn_prk_adj$Trade.IndexAdj <-
#  chn_prk_adj$Trade.Index -
#  part_year_chn$Trade.Index[part_year_chn$Trade.Flow != "Re-Import" & part_year_chn$Partner.ISO == "WLD"]

#chn_prk_spread <-
#  chn_prk_adj %>%
#  select(-Trade.Value, -Trade.Index) %>%
#  spread(Trade.Flow, Trade.IndexAdj) %>%
#  mutate(Year.Start = as.character(Year)) %>%
#  select(-Year)