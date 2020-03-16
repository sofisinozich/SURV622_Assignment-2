library(rtweet)
library(tidyverse)
library(magrittr)
library(quanteda)
library(ggplot2)
library(lubridate)

source("Scripts/load_dataviz_themes.R")


# Don't do this again unless something is wrong with the tokenized tweets!
# Concatenate all the streamed tweets
# ss1_tweets <- parse_stream("Data/ss_streamed_tweets.json")
# bs_tweets <- parse_stream("Data/bs_streamed_tweets_mar5-10am-mar8-1211pm.json")
# rj1_tweets <- parse_stream("Data/ss_streamed_tweets_RJ2.json")
# rj2_tweets <- parse_stream("Data/ss_streamed_tweets_RJ3.json")
# rj3_tweets <- parse_stream("Data/ss_streamed_tweets_RJ4.json")
# ss2_tweets <- parse_stream("Data/ss_mar8-mar13_tweets.json")
# 
# 
# maryland_tweets <- bind_rows(ss1_tweets,bs_tweets,rj1_tweets,rj2_tweets,rj3_tweets,ss2_tweets,.id="session") %>% distinct(status_id,.keep_all = TRUE)
# maryland_tweets %<>%
#   mutate(tokenized = map(text,function(x) x %>% tolower %>% tokens %>% pluck(1)))
# 
# write_rds(maryland_tweets, "Data/tokenized_tweets.rds")

maryland_tweets <- read_rds("Data/tokenized_tweets.rds")

# Filter irrelevant tweets

irrelevant <- read_csv("Data/irrelevant.csv",col_names=FALSE) %>% pull

maryland_tweets %<>% rowwise %>% mutate(irrelevance=tokenized %>% intersect(.,irrelevant) %>% length)
maryland_tweets %<>%    
  mutate(irrelevance = ifelse((c("women's","womens") %>% intersect(.,tokenized) %>% length>0) & (c("mens","men's") %>% intersect(.,tokenized) %>% length==0), irrelevance+1,irrelevance))

relevant_tweets <- maryland_tweets %>% filter(irrelevance == 0)

relevant_tweets %<>% mutate(created_at_eastern = created_at %>% with_tz("America/New_York"))

# Save relevant tweets to a new RDS file

  write_rds(relevant_tweets, "Data/tokenized_relevant_tweets.rds")