library(quanteda)
library(dplyr)
library(magrittr)
library(ggplot2)

##########
#  Data  #
##########

# The following data was downloaded from the UN website.
# It consists of speeches by Chinese representatives in the Security Council on DPRK topics.
# Text and metadata from each meeting's PDFs was copied into a CSV table, then quanteda objects were formed.

unsc_meeting_file <- "~/Documents/rprojects/poli199/data/china-unsc-nkmeetings.RData"
load(unsc_meeting_file)

raw_meet_tok <- meet_tok
meet_tok %<>% tokens_remove(stopwords('en'))
compounds <- c("Terminal High Altitude Area Defense",
               "Democratic People's Republic Korea",
               "Korean peninsula",
               "Republic Korea",
               "Six-Party Talks",
               "North-East Asia*",
               "North - East Asia*",
               "Security Council",
               "United Nations")
meet_tok %<>% tokens_compound(phrase(compounds))

# Ideally the data would be compiled using the following structures:

# A. Directory tree of speech data
#   - dir: meeting_ID
#   - file:
#     - name: Country
#     - contents: Speech data

# B. Table of UN Security Council meetings
#   1. meeting_ID
#   2. Date
#   3. Topic

# C. Lookup table for speakers and countries

# The directory tree can be imported using the following readtext call.
#
#   readtext("SPEECHDATA/*", docvarsfrom="filepaths", dvsep="/", docvarnames=c("meeting_ID", "Country"))
#
# The other tables can be compiled manually from information in the document, then the structures amalgamated.

##################
# Basic Analysis #
##################

# first, let's form some keyword-in-contexts using `kwic()`

missile_kwic <- kwic(meet_tok, "missile*", window=20)

# Some notable excerpts:
#
#   Ever since discussions started in the Security Council on the launching of 	missiles
#   by the Democratic People's Republic of Korea , China has acted in a persistent manner
#   to achieve two major objectives
#
#   We are opposed to the deployment of the Terminal High Altitude Area Defense anti-ballistic 	missile
#   system on the peninsula , as it seriously undermines the strategic security interests of China

interest_keywords <- c('interest*',
                       'objective*',
                       'concern*',
                       'importan*',
                       'goal*')

interest_kwic <- meet_tok %>% kwic(interest_keywords, window=12)

term_frequency <- function(terms) {
  terms %<>%
    lapply(strsplit, ' ') %>%
    unlist()
  as.data.frame(terms, row.names=NULL) %>%
    count(terms)
}

pre_interest <- term_frequency(interest_kwic$pre)
post_interest <- term_frequency(interest_kwic$post)

# qualititative analysis for top terms using pre_interest and post_interest

top_interests <- c("peace",
                   "stability",
                   "stable",
                   "security",
                   "denucleariz*")

# now let's look for mentions of these interests across the texts
interests_intexts <- meet_tok %>% kwic(top_interests, window=0)

# clean mentions of interests so similar words are grouped
# convert docnames to numeric values, so they can be used to reference data in the corpus
# finally, tally mentions per document with count()
interests_intexts %<>% mutate(keyword = tolower(keyword) %>%
                                gsub("denucleariz.*", "denuclearization", .) %>%
                                gsub("stable", "stability", .),
                              docname = as.numeric(gsub("text", "", docname))) %>%
                       count(docname, keyword)

# bring in variables from the corpus
interests_intexts %<>% mutate(word_count = str_count(meet_corp[[docname, "texts"]], '\\w+'),
                              date = meet_corp[[docname, "date"]],
                              speaker = meet_corp[[docname, "speaker"]],
                              topic = meet_corp[[docname, "topic"]])

# visualise the raw data
interests_intexts %>% ggplot(aes(x=docname, y=n, fill=keyword)) + geom_col(position="fill")

# visualise based on topic and speaker
interests_intexts %>% ggplot(aes(x=topic, y=n, fill=keyword)) + geom_col(position="fill") + coord_flip()
interests_intexts %>% ggplot(aes(x=speaker, y=n, fill=keyword)) + geom_col(position="fill") + coord_flip()

# account for texts of different lengths with the word_count variable
interests_intexts %>% ggplot(aes(x=docname, y=n/word_count, color=keyword)) + geom_jitter(alpha=0.67, size=4, width=0, height=0.0001) + geom_line()
interests_intexts %>% ggplot(aes(x=date, y=n/word_count, color=keyword)) + geom_jitter(alpha=0.67, size=4, width=0, height=0.001)

interests_intexts %>% filter(date > "2014-01-01") %>% ggplot(aes(x=date, y=n/word_count, color=keyword)) + geom_jitter(alpha=0.67, size=4, width=0, height=0.001) + geom_smooth(se=F)

