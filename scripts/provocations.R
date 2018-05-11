library(dplyr)
library(magrittr)
library(ggplot2)

prov <- read.csv("data/provocations.csv")
prov %<>%
  mutate(Date = as.character(Date),
         Description = as.character(Description),
         Resources = as.character(Resources))
prov$Date[[133]] <- "2008-03-02 - 2008-03-07"

prov %<>%
  mutate(Date.Start = as.Date(trimws(gsub("\\s+-\\s+.*", "", as.character(Date)))),
         Date.End = as.Date(trimws(gsub(".*\\s+-\\s+", "", as.character(Date))))) %>%
  select(-Date) %>%
  filter(Date.Start > as.Date("2003-01-01"))

prov_per_year <- function(df) {
  df %>%
    mutate(Year.Start = format(Date.Start, "%Y")) %>%
    count(Year.Start)
}

prov %>%
  ggplot(aes(x=format(Date.Start, "%Y"), fill=Type)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Korean Peninsula Provocations") +
  xlab("Year") + ylab("Number of Provocations")

ggsave("output/provocation_years.svg", device="svg")
