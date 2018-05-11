library(dplyr)
library(ggplot2)

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
part_year <- all %>% group_by(Partner, Year) %>% summarise(trade_usd = sum(Trade.Value))
part_year$trade_portion <- part_year$trade_usd / part_year$trade_usd[part_year$Partner == "World"]
part_year_chn <- all %>% filter(Reporter.ISO == "CHN") %>%
                         group_by(Partner.ISO, Year, Trade.Flow) %>%
                         summarise(Trade.Value = sum(Trade.Value))

# DPRK raw value
part_year %>% filter(Partner == "Dem. People's Rep. of Korea") %>% ggplot(aes(y=trade_usd, x=Year)) + geom_line()
# All partners, raw
part_year %>% ggplot(aes(y=trade_usd, x=Year, color=Partner)) + geom_line()
# USA, ROK, Japan relative to world
part_year %>% filter(Partner != "World", Partner != "Dem. People's Rep. of Korea") %>%
  ggplot(aes(y=trade_portion, x=Year, color=Partner)) + geom_line()

part_year_chn %>%
  ggplot(aes(y=Trade.Value, x=Year, color=Partner.ISO)) +
    geom_point(size=1.8) +
    facet_wrap(~Trade.Flow)

part_year_chn %>%
  filter(Partner.ISO != "WLD", Partner.ISO != "PRK") %>%
  ggplot(aes(y=Trade.Value, x=Year, color=Partner.ISO)) +
    geom_point(size=2.2222) +
    facet_wrap(~Trade.Flow)

part_year_chn %>%
  filter(Partner.ISO == "PRK") %>%
  ggplot(aes(y=Trade.Value, x=Year, color=Trade.Flow)) + geom_point(size=3.3333)
