source("scripts/trade-data.R")

# NOT CHINA SPECIFIC
# DPRK raw value
#part_year %>% filter(Partner == "Dem. People's Rep. of Korea") %>% ggplot(aes(y=trade_usd, x=Year)) + geom_line()
# All partners, raw
#part_year %>% ggplot(aes(y=trade_usd, x=Year, color=Partner)) + geom_line()
# USA, ROK, Japan relative to world
#part_year %>% filter(Partner != "World", Partner != "Dem. People's Rep. of Korea") %>%
#  ggplot(aes(y=trade_portion, x=Year, color=Partner)) + geom_line()

long_adj_index %>%
  ggplot(aes(y=Trade.Value/1000000000000, x=Year, color=Partner.ISO)) +
  geom_point(size=1.8) +
  facet_wrap(~Trade.Flow) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Trade Value (trillions of USD)") +
  ggtitle("Exports to US Significant on a Global Scale")

ggsave("output/china_trade_total.svg", width=5.25, height=3.33, device="svg")

long_adj_index %>%
  filter(Partner.ISO != "WLD", Partner.ISO != "PRK") %>%
  ggplot(aes(y=Trade.Index, x=Year, color=Partner.ISO)) +
  geom_point(size=2.2222) +
  facet_wrap(~Trade.Flow) +
  ylab("Percent Growth") +
  ggtitle("Trade with Japan falls behind US and RoK")

ggsave("output/china_trade_us_rok_jpn.svg", width=5.25, height=3.33, device="svg")

long_adj_index %>%
  filter(Partner.ISO == "PRK" | Partner.ISO == "WLD") %>%
  ggplot(aes(y=Trade.Index, x=Year, color=Partner.ISO)) +
  geom_point(size=2.2222) +
  facet_wrap(~Trade.Flow) +
  ylab("Percent Growth") +
  ggtitle("China-DPRK Trade Growing Slowly")

ggsave("output/china_trade_dprk.svg", width=5.25, height=3.33, device="svg")
