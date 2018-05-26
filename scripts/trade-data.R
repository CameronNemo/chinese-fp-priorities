source("scripts/common.R")

early <- read.csv("data/comtrade0307.csv")
mid <- read.csv("data/comtrade0812.csv")
late <- read.csv("data/comtrade1317.csv")
supplement <- read.csv("data/comtrade17chn.csv")

clean_comtrade_totals <- function(.data) {
  .data %>%
    mutate(Trade.Value = Trade.Value..US..) %>%
    select(Year, Reporter, Reporter.ISO, Partner, Partner.ISO, Trade.Flow, Trade.Value)
}

early <- clean_comtrade_totals(early)
mid <- clean_comtrade_totals(mid)
late <- clean_comtrade_totals(late)
supplement <- clean_comtrade_totals(supplement)

all <- rbind(early, mid, late)

# TODO: 2017supplement, check portions
#part_year <- all %>% group_by(Partner, Year) %>% summarise(trade_usd = sum(Trade.Value))
#part_year$trade_portion <- part_year$trade_usd / part_year$trade_usd[part_year$Partner == "World"]
part_year_chn <- all %>% filter(Reporter.ISO == "CHN") %>%
                         group_by(Partner.ISO, Year, Trade.Flow) %>%
                         summarise(Trade.Value = sum(Trade.Value))
# get indexed trading patterns
part_year_chn %<>%
  group_by(Partner.ISO, Trade.Flow) %>%
  mutate(Trade.Index = ((Trade.Value / Trade.Value[Year == min(Year)]) - 1) * 100)

# adjust PRK trade index for global trends
chn_prk_adj <-
  part_year_chn %>%
  filter(Partner.ISO == "PRK")
chn_prk_adj$Trade.IndexAdj <-
  chn_prk_adj$Trade.Index -
  part_year_chn$Trade.Index[part_year_chn$Trade.Flow != "Re-Import" & part_year_chn$Partner.ISO == "WLD"]

chn_prk_spread <-
  chn_prk_adj %>%
  select(-Trade.Value, -Trade.Index) %>%
  spread(Trade.Flow, Trade.IndexAdj) %>%
  mutate(Year.Start = as.character(Year)) %>%
  select(-Year)