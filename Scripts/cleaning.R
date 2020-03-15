library(rtweet)
library(tidyverse)
library(magrittr)
library(quanteda)

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
# maryland_tweets <- bind_rows(ss1_tweets,bs_tweets,rj1_tweets,rj2_tweets,rj3_tweets,ss2_tweets,.id="session")
# maryland_tweets %<>%
#   mutate(tokenized = map(text,function(x) x %>% tolower %>% tokens %>% pluck(1)))
#  
# write_rds(maryland_tweets, "Data/tokenized_tweets.rds")

read_rds("Data/tokenized_tweets.rds")

# Filter irrelevant tweets
# Ex. tweets including "high school" are likely irrelevant (relating to MD high school basketball)

irrelevant <- read_csv("Data/irrelevant.csv",col_names=FALSE) %>% pull