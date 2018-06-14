source("scripts/unsc-meetings.R")

interests_intexts %>%
  filter(date > "2014-01-01") %>%
  ggplot(aes(x=date, y=n/word_count, color=keyword)) +
  geom_jitter(alpha=0.67, size=4, width=0, height=0.0008) +
  geom_smooth(method=glm, se=F) +
  ggtitle("Fig. 5: Rising Importance of Denuclearisation") +
  ylab("Relative Frequency") + xlab("Year")

ggsave("output/china_unsc_interests.svg", height=4.04, width=5.35, device="svg")

# visualise the raw data
#interests_intexts %>% ggplot(aes(x=as.factor(docname), y=n, fill=keyword)) + geom_col(position="fill")

# visualise based on topic and speaker
#interests_intexts %>% ggplot(aes(x=topic, y=n, fill=keyword)) + geom_col(position="fill") + coord_flip()
#interests_intexts %>% ggplot(aes(x=speaker, y=n, fill=keyword)) + geom_col(position="fill") + coord_flip()

# account for texts of different lengths with the word_count variable
#interests_intexts %>% ggplot(aes(x=docname, y=n/word_count, color=keyword)) + geom_jitter(alpha=0.67, size=4, width=0, height=0.0001) + geom_line()
#interests_intexts %>% ggplot(aes(x=date, y=n/word_count, color=keyword)) + geom_jitter(alpha=0.67, size=4, width=0, height=0.001)