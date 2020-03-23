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
density_plain <- ggplot(relevant_tweets_sentiment,aes(x=SentimentGI)) + 
  geom_density(alpha=.5, position="identity") + ggtitle("All tweets")+ylab("Density")
density_overlay <- ggplot(relevant_tweets_sentiment,aes(x=SentimentGI,fill=tweet_type)) + 
  geom_density(alpha=.5, position="identity") +
  ylab("") +
  theme(legend.title = element_blank())
# ggsave("Plots/density_tweet_type_overlay.png",density_overlay)

density3 <- relevant_tweets_sentiment %>% group_by(tweet_type) %>% 
  mutate(SentimentGI_mean = mean(SentimentGI,na.rm=TRUE), SentimentGI_median = median(SentimentGI,na.rm=TRUE)) %>% 
  ggplot(aes(x=SentimentGI)) + 
  geom_density(alpha=.5, position="identity") +
  geom_vline(aes(xintercept=SentimentGI_mean),color = "red") +
  geom_vline(aes(xintercept=SentimentGI_median), color = "blue") +
  geom_text(data= . %>% slice(1), aes(label = paste0("mean: ",round(SentimentGI_mean,2)), x = SentimentGI_mean + 0.25), y = 4,color = "red") +
  geom_text(data= . %>% slice(1), aes(label = paste0("median: ",round(SentimentGI_median,2)), x = SentimentGI_median -0.25), y = 6,  color = "blue") +
  ylab("Density")+
  facet_grid(tweet_type~.) 
# ggsave("Plots/density_tweet_type3.png",density3)

# ggsave("Plots/density_2up.png",grid.arrange(density3,density_overlay,ncol=2))

# By category (-1,0,1)
frequency3 <- relevant_tweets_sentiment %>% mutate(sentiment_category = case_when(SentimentGI > 0 ~ "Positive", SentimentGI < 0 ~ "Negative", SentimentGI == 0 ~ "Neutral")) %>% 
  group_by(tweet_type,sentiment_category) %>% summarize(n = n()) %>% mutate(percent = n/sum(n)) %>% select(tweet_type,sentiment_category,n,percent) %>% 
  filter(!is.na(sentiment_category)) %>% 
  ggplot() + geom_bar(aes(x=sentiment_category,y=n),stat="identity") + 
  geom_text(aes(x=sentiment_category,y=n+100,label=paste0(n," (",round(percent,2)*100,"%)")))+
  xlab("Sentiment Category") + ylab("Count")+
  facet_wrap(.~tweet_type)
# ggsave("Plots/frequency_tweet_type3.png",frequency3)


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
relevant_tweets_sentiment %>% filter(is_retweet) %>% group_by(retweet_status_id) %>% mutate(count = n()) %>% 
  distinct(retweet_status_id,.keep_all=TRUE) %>% select(retweet_text,count,retweet_screen_name,SentimentGI) %>% arrange(desc(count)) 

# Most quoted tweet
relevant_tweets_sentiment %>% filter(is_quote & !is_retweet) %>% group_by(quoted_status_id) %>% mutate(count = n()) %>% 
  distinct(quoted_status_id,.keep_all=TRUE) %>% select(quoted_text,count) %>% arrange(desc(count))

# Most positive by each
relevant_tweets_sentiment %>% group_by(tweet_type) %>% 
  summarize(most_pos = max(SentimentGI,na.rm=TRUE), most_neg = min(SentimentGI,na.rm=TRUE))


# RTs ---------------------------------------------------------------------
retweets <- relevant_tweets_sentiment %>% 
  filter(is_retweet) %>% 
  select(-text) %>% rename(text = retweet_text) %>% 
  bind_cols(sentimenter(.))

retweets %>% group_by(retweet_screen_name) %>% 
  summarize(n = n(), distinct_tweets = unique(retweet_status_id) %>% length) %>% mutate(percent = n/sum(n)) %>% 
  arrange(desc(n))

# How many of these are UMD-centric?
retweets %>% group_by(retweet_screen_name) %>% 
  summarize(n = n(), distinct_tweets = unique(retweet_status_id) %>% length) %>% 
  mutate(percent = n/sum(n)) %>% 
  filter(grepl("terps|terrapin|umd|maryland|testudo",tolower(retweet_screen_name))) %>% summarize(percent=sum(percent))

# Positivity x number of RTs
retweets %>% group_by(retweet_status_id) %>% mutate(count = n()) %>% 
  select(SentimentGI,count) %>% distinct(retweet_status_id,.keep_all=TRUE) %>% 
  ggplot() + geom_point(aes(x=SentimentGI,y=count))


# QRTs --------------------------------------------------------------------
# Sentiment of the quoted content?
quote_tweets <- relevant_tweets_sentiment %>% 
  filter(is_quote & !is_retweet) %>% # Quote + RT = it was a quoted tweet that the user RT'd
  select(-text) %>% rename(text = quoted_text) %>% 
  bind_cols(sentimenter(.))

# "Weighted" by frequency
summary(quote_tweets$SentimentGI)
qrts_raw <- ggplot(quote_tweets,aes(x=SentimentGI)) + 
  geom_density(alpha=.5, position="identity")+
  ggtitle("All quoted tweets")+
  ylab("")

# Unique only
summary(quote_tweets %>% distinct(quoted_status_id,.keep_all=TRUE) %>% select(SentimentGI))
qrts_unique <- quote_tweets %>% distinct(quoted_status_id,.keep_all=TRUE) %>% select(SentimentGI) %>% 
  ggplot(aes(x=SentimentGI)) + 
  geom_density(alpha=.5, position="identity") +
  ylab("")+
  ggtitle("Unique quoted tweets")

ggsave("Plots/density_qrts.png",grid.arrange(density_plain,qrts_raw,qrts_unique,ncol=3))

# Top QRT producers
quote_tweets %>% group_by(quoted_screen_name) %>% 
  summarize(n = n(), distinct_tweets = unique(quoted_status_id) %>% length) %>% 
  arrange(desc(n))

# Sentiment of the quote vs. sentiment of the text
# SentimentGI1 is the quote, SentimentGI is the original text
density_textquote<-quote_tweets %>% 
  left_join(relevant_tweets_sentiment %>% filter(is_quote & !is_retweet) %>% select(text,status_id), by=c("status_id")) %>% 
  select(text=text.y,textsentiment=SentimentGI,quoted=text.x,quotedsentiment=SentimentGI1,quotedauthor = quoted_screen_name) %>% 
  ggplot() +
  geom_density(aes(x=textsentiment,fill="Text"),alpha=.5) +
  geom_density(aes(x=quotedsentiment,fill="Quoted text"),alpha=.5)+
  xlab("Sentiment")+ylab("Density")+
  theme(legend.title = element_blank())
# ggsave("Plots/density_text_quoted.png",density_textquote)

# check with LCE
quote_tweets %>% 
  left_join(relevant_tweets_sentiment %>% filter(is_quote & !is_retweet) %>% select(text,status_id), by=c("status_id")) %>% 
  select(text=text.y,textsentiment=SentimentLCE,quoted=text.x,quotedsentiment=SentimentLCE1,quotedauthor = quoted_screen_name) %>% 
  ggplot() +
  geom_density(aes(x=textsentiment,fill="Text"),alpha=.5) +
  geom_density(aes(x=quotedsentiment,fill="Quoted"),alpha=.5)+
  xlab("Sentiment")+
  scale_fill_discrete(name = "Sentiment")


textvquotedsentiment <- quote_tweets %>% 
  left_join(relevant_tweets_sentiment %>% filter(is_quote & !is_retweet) %>% select(text,status_id), by=c("status_id")) %>% 
  select(text=text.y,textsentiment=SentimentGI,quoted=text.x,quotedsentiment=SentimentGI1,quotedauthor = quoted_screen_name) %>% 
  mutate(terphoops = case_when(quotedauthor == "TerrapinHoops"~"Quote by @TerrapinHoops",TRUE ~ "All others")) %>% 
  ggplot() +
  geom_point(aes(x=quotedsentiment,y=textsentiment,color=terphoops)) +
  xlab("Quoted text sentiment") + ylab("Text sentiment")+
  scale_color_manual(values=c(.maryland_red,"black"))+
  theme(legend.title = element_blank())+
  guides(color = guide_legend(reverse=TRUE))
ggsave("Plots/text_quoted_sentiment_scatter.png",textvquotedsentiment)

# Mean/median reaction to most popular
quote_tweets %>% 
  group_by(quoted_status_id) %>% 
  mutate(mean=mean(SentimentGI,na.rm=TRUE),median=median(SentimentGI,na.rm=TRUE),count=n(),quoted_status_url = paste0("https://twitter.com/i/web/status/",quoted_status_id)) %>% 
  distinct(quoted_status_id,.keep_all=TRUE) %>% select(text,mean,median,count,quoted_screen_name,quoted_status_url) %>% 
  ungroup %>% arrange(desc(count)) %>% slice(1:10) %>% 
  select(quoted_status_url) %>% pull
