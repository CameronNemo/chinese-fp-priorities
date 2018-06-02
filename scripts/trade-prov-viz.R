source("scripts/trade-prov.R")

trade_prov %>%
  ggplot(aes(x=as.factor(MilitaryExercise), y=JPN.Export+JPN.Import)) +
  geom_boxplot()

trade_prov %>%
  ggplot(aes(x=MilitaryExercise, y=JPN.Export+JPN.Import)) +
  geom_point() +
  geom_smooth(method=glm, se=F)

trade_prov %>%
  ggplot(aes(x=MilitaryExercise, y=KOR.Export+KOR.Import)) +
  geom_point() +
  geom_smooth(method=glm, se=F)
