library(rtweet)
library(tidyverse)
library(magrittr)
library(quanteda)
library(ggplot2)
library(lubridate)

source("Scripts/load_dataviz_themes.R")

# Import a table of events that can be used for contextualizing analysis
# (e.g. timing of press release cancelling the Big Ten tournament)
  contextual_events <- read_csv("data/contextual-events-timeline.csv") %>%
    mutate(Time = ymd_hms(Time, tz = "America/New_York"))

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


# Plots -------------------------------------------------------------------

ggplot(data=relevant_tweets)+ geom_bar(aes(x=as_date(created_at_eastern)),stat="count") + 
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d\n%a") +
  annotate("text",y=5000, x=as.Date("2020-03-11"), label = "← 3/8 Michigan @ Maryland") +
  annotate("text",y=3000, x=as.Date("2020-03-02"), label = "↓ 2/29 Michigan State @ Maryland") +
  annotate("text",y=1000, x=as.Date("2020-03-03"), label = "↓ No data collected ~3/2-3/4\nincludes UMD @ Rutgers", size = 3) +
  annotate("text",y=1500, x=as.Date("2020-03-12"), label = "↓ Apparent disconnect 3/11", size=3) +
  labs(x="Date",y="Relevant tweets\ncollected")

ggplot(data=relevant_tweets %>% mutate(hours = created_at_eastern %>% hour))+
  geom_line(aes(x=hours), stat="count")

# Tweets by day of week and time of day
  relevant_tweets %>%
    mutate(hours = hour(created_at_eastern),
           day_of_week = wday(created_at_eastern, label = TRUE, abbr = FALSE)) %>%
    ggplot(aes(x = hours))+
    geom_line(stat="count") +
    lemon::facet_rep_wrap(~ day_of_week, repeat.tick.labels = 'x') +
    scale_x_continuous(breaks = seq(0, 24, 4)) +
    labs(
      title = "Tweets by day of week and time of day",
      x = "Time of Day",
      y = "# of tweets\ncreated"
    )
