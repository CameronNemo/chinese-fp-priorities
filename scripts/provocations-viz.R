source("scripts/provocations.R")

prov %>%
  ggplot(aes(x=format(Date.Start, "%Y"), fill=Type)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Fig. 4: Growth in Missile Provocations") +
  xlab("Year") + ylab("Number of Provocations")

ggsave("output/provocation_years.svg", width=4.84, height=3.66, device="svg")

#prov %>%
#  prov_per_year() %>%
#  ggplot(aes(x=Type, y=n)) +
#  geom_boxplot() +
#  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
#  ylab("Number of Events per Year") +
#  ggtitle("Annual Provocations by Provocation Type")

#ggsave("output/provocation_types.svg", device="svg")

svg("output/provocation_types_table.svg", height=1.8, width=3.9)
prov %>%
  prov_per_year() %>%
  group_by(Type) %>%
  summarise(Mean=format(mean(n), digits=3),
            Median=format(median(n), digits=3),
            Std.Dev=format(sd(n), digits=3)) %>%
  grid.table(rows=NULL)
dev.off()
