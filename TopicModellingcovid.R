# data wrangling
library(dplyr)
library(tidyr)
library(lubridate)
# visualization
library(ggplot2)
# dealing with text
library(textclean)
library(tm)
library(SnowballC)
library(stringr)
# topic model
library(tidytext)
library(topicmodels)
library(textmineR)

# data import and preparation
f <- file.choose("virusdata.csv")
data <- read.csv(f)
data <- data %>%
  mutate(overall = as.factor(overall),
         reviewTime = str_replace_all(reviewTime, pattern = " ",replacement = "-"),
         reviewTime = str_replace(reviewTime, pattern = ",",replacement = ""),
         reviewTime = mdy(reviewTime)) %>%
  select(reviewText, overall,reviewTime)
head(data)

# build textcleaner function
textcleaner <- function(x){
  x <- as.character(x)
  
  x <- x %>%
    str_to_lower() %>%  # convert all the string to low alphabet
    replace_contraction() %>% # replace contraction to their multi-word forms
    replace_internet_slang() %>% # replace internet slang to normal words
    replace_emoji() %>% # replace emoji to words
    replace_emoticon() %>% # replace emoticon to words
    replace_hash(replacement = "") %>% # remove hashtag
    replace_word_elongation() %>% # replace informal writing with known semantic replacements
    replace_number(remove = T) %>% # remove number
    replace_date(replacement = "") %>% # remove date
    replace_time(replacement = "") %>% # remove time
    str_remove_all(pattern = "[[:punct:]]") %>% # remove punctuation
    str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>% # remove mixed string n number
    str_squish() %>% # reduces repeated whitespace inside a string.
    str_trim() # removes whitespace from start and end of string
  
  xdtm <- VCorpus(VectorSource(x)) %>%
    tm_map(removeWords, stopwords("en"))
  
  # convert corpus to document term matrix
  return(DocumentTermMatrix(xdtm))
  data_1 <- data %>% filter(overall == 1)
  data_2 <- data %>% filter(overall == 2)
  data_3 <- data %>% filter(overall == 3)
  data_4 <- data %>% filter(overall == 4)
  data_5 <- data %>% filter(overall == 5)
  table(data$overall)
  
}
# apply textcleaner function for review text
dtm_5 <- textcleaner(data_5$reviewText)
# find most frequent terms. i choose words that at least appear in 50 reviews
freqterm_5 <- findFreqTerms(dtm_5,50)
# we have 981 words. subset the dtm to only choose those selected words
dtm_5 <- dtm_5[,freqterm_5]
# only choose words that appear once in each rows
rownum_5 <- apply(dtm_5,1,sum)
dtm_5 <- dtm_5[rownum_5>0,]
# apply to LDA function. set the k = 6, means we want to build 6 topic 
lda_5 <- LDA(dtm_5,k = 6,control = list(seed = 1502))
# apply auto tidy using tidy and use beta as per-topic-per-word probabilities
topic_5 <- tidy(lda_5,matrix = "beta")
# choose 15 words with highest beta from each topic
top_terms_5 <- topic_5 %>%
  group_by(topic) %>%
  top_n(15,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)
# plot the topic and words for easy interpretation
plot_topic_5 <- top_terms_5 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
plot_topic_5