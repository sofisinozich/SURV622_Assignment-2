library(rtweet)
library(tidyverse)
library(magrittr)
library(quanteda)
library(ggplot2)
library(lubridate)

source("Scripts/load_dataviz_themes.R")


# Don't do this again unless something is wrong with the tokenized tweets!
# Concatenate all the streamed tweets
# ss1_tweets <- parse_stream("Data/streamed/ss_streamed_tweets.json")
# bs_tweets <- parse_stream("Data/streamed/bs_streamed_tweets_mar5-10am-mar8-1211pm.json")
# rj1_tweets <- parse_stream("Data/streamed/ss_streamed_tweets_RJ2.json")
# rj2_tweets <- parse_stream("Data/streamed/ss_streamed_tweets_RJ3.json")
# rj3_tweets <- parse_stream("Data/streamed/ss_streamed_tweets_RJ4.json")
# ss2_tweets <- parse_stream("Data/streamed/ss_mar8-mar13_tweets.json")
# 
# 
# maryland_tweets <- bind_rows(ss1_tweets,bs_tweets,rj1_tweets,rj2_tweets,rj3_tweets,ss2_tweets,.id="session") %>% distinct(status_id,.keep_all = TRUE)
# maryland_tweets %<>%
#   mutate(tokenized = map(text,function(x) x %>% tolower %>% tokens))

# write_rds(maryland_tweets, "Data/tokenized_tweets.rds")

maryland_tweets <- read_rds("Data/tokenized_tweets.rds") %>% ungroup()

# Filter irrelevant tweets

irrelevant <- read_csv("Data/irrelevant.csv",col_names=FALSE) %>% pull

maryland_tweets %<>% mutate(irrelevance = grepl(paste0(irrelevant,collapse="|"),text %>% tolower) | (grepl("women's|womens",text %>% tolower) & !grepl("\\bmen's|\\bmens",text %>% tolower)))

relevant_tweets <- maryland_tweets %>% filter(irrelevance == FALSE)

relevant_tweets %<>% mutate_at(vars(matches("created_at$")),
                               list(`eastern` = function(x) with_tz(x, "America/New_York")))

# Save relevant tweets to a new RDS file

  write_rds(relevant_tweets, "Data/tokenized_relevant_tweets.rds")