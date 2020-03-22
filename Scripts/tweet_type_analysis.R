library(tidyverse)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(magrittr)
library(lemon)

relevant_tweets_sentiment <- read_rds("Data/tokenized_relevant_tweets_sentiment.rds")

# Categories - original, RTs (including RT'd quote RTs = is_retweet + is_quote), quote RTs
relevant_tweets_sentiment %>% count(is_retweet,is_quote)

relevant_tweets_sentiment %<>% mutate(tweet_type = 
                                       case_when(is_retweet ~ "RT",
                                                 is_quote ~ "QRT",
                                                 TRUE ~ "Original"))

# Summarized
relevant_tweets_sentiment %>% 
  group_by(tweet_type) %>% summarize_at(vars(starts_with("Sentiment")), mean, na.rm=TRUE)
relevant_tweets_sentiment %>% 
  group_by(tweet_type) %>% summarize_at(vars(starts_with("Sentiment")), median, na.rm=TRUE)

# Density plots
ggplot(relevant_tweets_sentiment,aes(x=SentimentGI,fill=tweet_type)) + 
  geom_density(alpha=.5, position="identity")

relevant_tweets_sentiment %>% group_by(tweet_type) %>% 
  mutate(SentimentGI_mean = mean(SentimentGI,na.rm=TRUE), SentimentGI_median = median(SentimentGI,na.rm=TRUE)) %>% 
  ggplot(aes(x=SentimentGI)) + 
  geom_density(alpha=.5, position="identity") +
  geom_vline(aes(xintercept=SentimentGI_mean),color = "red") +
  geom_vline(aes(xintercept=SentimentGI_median), color = "blue") +
  geom_text(data= . %>% slice(1), aes(label = paste0("mean: ",round(SentimentGI_mean,2)), x = SentimentGI_mean + 0.25), y = 4,color = "red") +
  geom_text(data= . %>% slice(1), aes(label = paste0("median: ",round(SentimentGI_median,2)), x = SentimentGI_median -0.25), y = 6,  color = "blue") +
  facet_grid(tweet_type~.)

# Term frequency by type
original_dfm <- tokens(relevant_tweets_sentiment %>% filter(tweet_type=="Original") %>%select(text) %>% corpus,remove_punct = TRUE,remove_url=TRUE,remove_separators=TRUE) %>% 
  tokens_remove(stopwords("english")) %>%
  tokens_remove("^[A-z]{1}$",valuetype="regex") %>% dfm

qrt_dfm <- tokens(relevant_tweets_sentiment %>% filter(tweet_type=="QRT") %>%select(text) %>% corpus,remove_punct = TRUE,remove_url=TRUE,remove_separators=TRUE) %>% 
  tokens_remove(stopwords("english")) %>%
  tokens_remove("^[A-z]{1}$",valuetype="regex") %>% dfm

rt_dfm <- tokens(relevant_tweets_sentiment %>% filter(tweet_type=="RT") %>%select(text) %>% corpus,remove_punct = TRUE,remove_url=TRUE,remove_separators=TRUE) %>% 
  tokens_remove(stopwords("english")) %>%
  tokens_remove("^[A-z]{1}$",valuetype="regex") %>% dfm

original_top_plot <- topfeatures(original_dfm) %>% enframe %>% slice(1:10) %>%  
  ggplot + geom_bar(aes(y=value,x=reorder(name,-value)),stat="identity") + 
  xlab("Top 10 Terms") + 
  ggtitle("Original Tweets") + ylab("")
qrt_top_plot <- topfeatures(qrt_dfm) %>% enframe %>% slice(1:10) %>%  
  ggplot + geom_bar(aes(y=value,x=reorder(name,-value)),stat="identity") +
  xlab("Top 10 Terms") +
  ggtitle("Quote RTs") + ylab("") 
rt_top_plot <- topfeatures(rt_dfm) %>% enframe %>% slice(1:10) %>%  
  ggplot + geom_bar(aes(y=value,x=reorder(name,-value)),stat="identity") + 
  xlab("Top 10 Terms") +
  ggtitle("RTs") + ylab("")

original_top_plot
qrt_top_plot
rt_top_plot


# grid.arrange(original_top_plot,
#              grid.arrange(qrt_top_plot,rt_top_plot,ncol=2), nrow=2)


# Most RT'd tweet
relevant_tweets_sentiment %>% filter(!is.na(retweet_status_id)) %>% group_by(retweet_status_id) %>% mutate(count = n()) %>% 
  distinct(retweet_status_id,.keep_all=TRUE) %>% select(retweet_text,count,retweet_screen_name,SentimentGI) %>% arrange(desc(count)) 

# Most quoted tweet
relevant_tweets_sentiment %>% filter(!is.na(quoted_status_id)) %>% group_by(quoted_status_id) %>% mutate(count = n()) %>% 
  distinct(quoted_status_id,.keep_all=TRUE) %>% select(quoted_text,count) %>% arrange(desc(count))


# RTs ---------------------------------------------------------------------
retweets <- relevant_tweets_sentiment %>% 
  filter(is_retweet) %>% 
  select(-text) %>% rename(text = retweet_text) %>% 
  bind_cols(sentimenter(.))

retweets %>% group_by(retweet_screen_name) %>% 
  summarize(n = n(), distinct_tweets = unique(retweet_status_id) %>% length) %>% 
  arrange(desc(n))


# QRTs --------------------------------------------------------------------
# Sentiment of the quoted content?
quote_tweets <- relevant_tweets_sentiment %>% 
  filter(is_quote & !is_retweet) %>% # Quote + RT = it was a quoted tweet that the user RT'd
  select(-text) %>% rename(text = quoted_text) %>% 
  bind_cols(sentimenter(.))

# "Weighted" by frequency
summary(quote_tweets$SentimentGI)
ggplot(quote_tweets,aes(x=SentimentGI)) + 
  geom_density(alpha=.5, position="identity")+
  ggtitle("Sentiment of all quoted tweets")

# Unique only
summary(quote_tweets %>% distinct(quoted_status_id,.keep_all=TRUE) %>% select(SentimentGI))
quote_tweets %>% distinct(quoted_status_id,.keep_all=TRUE) %>% select(SentimentGI) %>% 
ggplot(aes(x=SentimentGI)) + 
  geom_density(alpha=.5, position="identity") +
  ggtitle("Sentiment of unique quoted tweets")

# Top QRT producers
quote_tweets %>% group_by(quoted_screen_name) %>% 
  summarize(n = n(), distinct_tweets = unique(quoted_status_id) %>% length) %>% 
  arrange(desc(n))

# Sentiment of the quote vs. sentiment of the text
# SentimentGI1 is the quote, SentimentGI is the original text
quote_tweets %>% 
  left_join(relevant_tweets_sentiment %>% filter(is_quote & !is_retweet) %>% select(text,status_id), by=c("status_id")) %>% 
  select(text=text.y,textsentiment=SentimentGI,quoted=text.x,quotedsentiment=SentimentGI1) %>% sample_n(10)

