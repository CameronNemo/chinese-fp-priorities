library(dplyr)
library(ggplot2)

early <- read.csv("data/comtrade0307.csv")
mid <- read.csv("data/comtrade0812.csv")
late <- read.csv("data/comtrade1317.csv")

clean <- function(df) {
  df %>% mutate(Trade.Value = Trade.Value..US..) %>% select(Classification, Year, Period, Trade.Flow, Reporter, Reporter.ISO, Partner, Partner.ISO, Trade.Value)
}

early <- clean(early)
mid <- clean(mid)
late <- clean(late)

all <- rbind(early, mid, late)

part_year <- all %>% group_by(Partner, Year) %>% summarise(trade_usd = sum(Trade.Value))
part_year$trade_portion <- part_year$trade_usd / part_year$trade_usd[part_year$Partner == "World"]

# DPRK raw value
part_year %>% filter(Partner == "Dem. People's Rep. of Korea") %>% ggplot(aes(y=trade_usd, x=Year)) + geom_line()
# All partners, raw
part_year %>% ggplot(aes(y=trade_usd, x=Year, color=Partner)) + geom_line()
# USA, ROK, Japan relative to world
part_year %>% filter(Partner != "World", Partner != "Dem. People's Rep. of Korea") %>%
  ggplot(aes(y=trade_portion, x=Year, color=Partner)) + geom_line()
