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
  ggplot(aes(y=Trade.Value/1000000000000, x=Year, color=Partner)) +
  geom_line(size=1) +
  facet_wrap(~Trade.Flow) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Trade Value (trillions of USD)") +
  ggtitle("Fig. 1: Exports to US Significant on a Global Scale")

ggsave("output/china_trade_total.svg", width=5.25, height=3.33, device="svg")

long_adj_index %>%
  filter(Partner.ISO != "WLD", Partner.ISO != "PRK") %>%
  ggplot(aes(y=Trade.Index, x=Year, color=Partner)) +
  geom_line(size=2.2222) +
  facet_wrap(~Trade.Flow) +
  ylab("Trade Value (indexed to 1993)") +
  ggtitle("Fig. 2: Trade with Japan falls behind US and RoK")

ggsave("output/china_trade_us_rok_jpn.svg", width=5.25, height=3.33, device="svg")

long_adj_index %>%
  filter(Partner.ISO == "PRK" | Partner.ISO == "WLD") %>%
  ggplot(aes(y=Trade.Index, x=Year, color=Partner)) +
  geom_line(size=2.2222) +
  facet_wrap(~Trade.Flow) +
  ylab("Trade Value (indexed to 1993)") +
  ggtitle("Fig 3: China-DPRK Trade Growing Slowly")

ggsave("output/china_trade_dprk.svg", width=5.25, height=3.33, device="svg")

long_growth %>%
  filter(Partner.ISO != "PRK") %>%
  ggplot(aes(x=Trade.Growth, color=Trade.Flow, fill=Trade.Flow)) +
  geom_density(alpha=0.3) +
  facet_wrap(~Partner)

dex <-
  long_growth %>%
  filter(Partner.ISO == "PRK", Trade.Flow == "Export")

dex %>%
  ggplot(aes(x=Trade.Growth)) +
  geom_density(alpha=0.3, fill="black", color="black") +
  geom_vline(aes(xintercept=median(Trade.Growth), col="Median"), size=1.3) +
  geom_vline(aes(xintercept=mean(Trade.Growth), col="Mean"), size=1.3) +
  stat_function(fun=dnorm, size=1.3,
                mapping=aes(color="Normal Distribution"),
                args=list(mean = mean(dex$Trade.Growth),
                          sd = sd(dex$Trade.Growth))) +
  guides(colour=guide_legend(title="Statistic")) +
  xlab("Annual Percent Growth in Exports") +
  ylab("Density") +
  ggtitle("Distribution of Chinese Export Growth to DPRK")

wide_growth %>%
  ggplot(aes(x=JPN.Export+JPN.Import)) +
  geom_density(alpha=0.3, fill="black", color="black") +
  geom_vline(aes(xintercept=median(JPN.Export+JPN.Import), col="Median"), size=1.3) +
  geom_vline(aes(xintercept=mean(JPN.Export+JPN.Import), col="Mean"), size=1.3) +
  stat_function(fun=dnorm, size=1.3,
                mapping=aes(color="Normal Distribution"),
                args=list(mean = mean(wide_growth$JPN.Export+wide_growth$JPN.Import),
                          sd = sd(wide_growth$JPN.Export+wide_growth$JPN.Import))) +
  guides(colour=guide_legend(title="Statistic")) +
  xlab("Annual Percent Growth") +
  ylab("Density") +
  ggtitle("Distribution of Total Chinese Trade Growth with Japan")

wide_growth %>%
  ggplot(aes(x=KOR.Export+KOR.Import)) +
  geom_density(alpha=0.3, fill="black", color="black") +
  geom_vline(aes(xintercept=median(KOR.Export+KOR.Import), col="Median"), size=1.3) +
  geom_vline(aes(xintercept=mean(KOR.Export+KOR.Import), col="Mean"), size=1.3) +
  stat_function(fun=dnorm, size=1.3,
                mapping=aes(color="Normal Distribution"),
                args=list(mean = mean(wide_growth$KOR.Export+wide_growth$KOR.Import),
                          sd = sd(wide_growth$KOR.Export+wide_growth$KOR.Import))) +
  guides(colour=guide_legend(title="Statistic")) +
  xlab("Annual Percent Growth in Imports and Exports") +
  ylab("Density") +
  ggtitle("Fig. 6: Distribution of Trade Growth with RoK")

ggsave("output/china_trade_rok_dist.svg", height=3.4, width=5, device="svg")
