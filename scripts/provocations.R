source("scripts/common.R")

prov <- read.csv("data/provocations.csv")
prov %<>%
  mutate(Date = as.character(Date),
         Description = as.character(Description),
         Resources = as.character(Resources))
# fix bad entry
prov$Date[[133]] <- "2008-03-02 - 2008-03-07"

prov %<>%
  mutate(Date.Start = as.Date(trimws(gsub("\\s+-\\s+.*", "", as.character(Date)))),
         Date.End = as.Date(trimws(gsub(".*\\s+-\\s+", "", as.character(Date))))) %>%
  select(-Date) %>%
  filter(Date.Start > as.Date("2003-01-01"))

prov_per_year <- function(.data) {
  .data %>%
    mutate(Year.Start = format(Date.Start, "%Y")) %>%
    count(Type, Year.Start) %>%
    complete(Type, Year.Start, fill=list(n=0))
}
