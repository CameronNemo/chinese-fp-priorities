source("scripts/trade-data.R")

# NOT CHINA SPECIFIC
# DPRK raw value
#part_year %>% filter(Partner == "Dem. People's Rep. of Korea") %>% ggplot(aes(y=trade_usd, x=Year)) + geom_line()
# All partners, raw
#part_year %>% ggplot(aes(y=trade_usd, x=Year, color=Partner)) + geom_line()
# USA, ROK, Japan relative to world
#part_year %>% filter(Partner != "World", Partner != "Dem. People's Rep. of Korea") %>%
#  ggplot(aes(y=trade_portion, x=Year, color=Partner)) + geom_line()

part_year_chn %>%
  filter(Trade.Flow != "Re-Import") %>%
  ggplot(aes(y=Trade.Value, x=Year, color=Partner.ISO)) +
  geom_point(size=1.8) +
  facet_wrap(~Trade.Flow) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Total Chinese Trade Flows")

ggsave("output/china_trade_total.svg", device="svg")

part_year_chn %>%
  filter(Partner.ISO != "WLD", Partner.ISO != "PRK") %>%
  ggplot(aes(y=Trade.Value, x=Year, color=Partner.ISO)) +
  geom_point(size=2.2222) +
  facet_wrap(~Trade.Flow) +
  ggtitle("Chinese Trade Flows with US, Japan, and RoK")

ggsave("output/china_trade_us_rok_jpn.svg", device="svg")

part_year_chn %>%
  filter(Partner.ISO == "PRK") %>%
  ggplot(aes(y=Trade.Value, x=Year, color=Trade.Flow)) +
  geom_point(size=3.3333) +
  ggtitle("China-DPRK Trade Flows")

ggsave("output/china_trade_dprk.svg", device="svg")