#Install qdap
#install.packages(qdap)

# Load data, packages, and any necessary functions
library(rtweet)
library(tidyverse)
library(magrittr)
library(quanteda)
library(ggplot2)
library(lubridate)
library(qdap)

source("Scripts/load_dataviz_themes.R")

relevant_tweets <- readRDS("Data/tokenized_relevant_tweets.rds")

# Identify most frequent words. Are any irrelevant?
frequent_terms_raw = freq_terms(relevant_tweets["text"], 30)
plot(frequent_terms_raw)

bagRelevant = relevant_tweets$text %>% 
  iconv("latin1", "ASCII", sub="") %>% 
  scrubber() %sw% 
  qdapDictionaries::Top200Words

frequent_terms_clean = freq_terms(bagRelevant, 30, stopwords = c("t", "co", "https", "b", "d", "g", "amp"))
plot(frequent_terms_clean)

# Note: "umdwbb" (related to women's basketball) is in the top 30 words. Need to add this to the irrelevant term list.

# Identify most frequent words before and after cancellation announced

relevant_tweets1 <- relevant_tweets %>%
  mutate(hours = hour(created_at_eastern),
         date = date(created_at_eastern)) %>%
  filter(date < as.Date("2020-03-12"))

frequent_terms_raw1 = freq_terms(relevant_tweets1["text"], 30)
plot(frequent_terms_raw1)

bagRelevant1 = relevant_tweets1$text %>% 
  iconv("latin1", "ASCII", sub="") %>% 
  scrubber() %sw% 
  qdapDictionaries::Top200Words

frequent_terms_clean1 = freq_terms(bagRelevant1, 30, stopwords = c("t", "co", "https", "b", "d", "g", "amp"))
plot(frequent_terms_clean1)

relevant_tweets2 <- relevant_tweets %>%
  mutate(hours = hour(created_at_eastern),
         date = date(created_at_eastern)) %>%
  filter(date >= as.Date("2020-03-12"))

frequent_terms_raw2 = freq_terms(relevant_tweets2["text"], 30)
plot(frequent_terms_raw2)

bagRelevant2 = relevant_tweets2$text %>% 
  iconv("latin1", "ASCII", sub="") %>% 
  scrubber() %sw% 
  qdapDictionaries::Top200Words

frequent_terms_clean2 = freq_terms(bagRelevant2, 30, stopwords = c("t", "co", "https", "b", "d", "g", "amp"))
plot(frequent_terms_clean2)

# Import a table of events that can be used for contextualizing analysis
# (e.g. timing of press release cancelling the Big Ten tournament)
  contextual_events <- read_csv("data/contextual-events-timeline.csv") %>%
    mutate(Time = ymd_hms(Time, tz = "America/New_York"))

# Plots -------------------------------------------------------------------

# Summary of results from streaming, with annotations for important events and gaps
  ggplot(data=relevant_tweets)+ geom_bar(aes(x=as_date(created_at_eastern)),stat="count") + 
    scale_x_date(date_breaks = "1 day", date_labels = "%b %d\n%a") +
    annotate("text",y=5000, x=as.Date("2020-03-11"), label = "← 3/8 Michigan @ Maryland") +
    annotate("text",y=3000, x=as.Date("2020-03-02"), label = "↓ 2/29 Michigan State @ Maryland") +
    annotate("text",y=1000, x=as.Date("2020-03-03"), label = "↓ No data collected ~3/2-3/4\nincludes UMD @ Rutgers", size = 3) +
    annotate("text",y=1500, x=as.Date("2020-03-12"), label = "↓ Apparent disconnect 3/11", size=3) +
    labs(x="Date",y="Relevant tweets\ncollected")

# Tweets by day of week and time of day
  relevant_tweets %>%
    mutate(hours = hour(created_at_eastern),
           day_of_week = wday(created_at_eastern, label = TRUE, abbr = FALSE),
           is_game_day = case_when(
             as.character(date(created_at_eastern)) %in% c("2020-02-29", "2020-03-03", "2020-03-08") ~ "Game day",
             TRUE ~ "Non-game day"
           )) %>%
    ggplot(aes(x = hours))+
    geom_line(aes(color = is_game_day), stat="count") +
    lemon::facet_rep_wrap(~ day_of_week, repeat.tick.labels = 'x') +
    scale_x_continuous(breaks = seq(0, 24, 4)) +
    scale_color_manual(name = "Type of day",
                       values = c("Game day" = .maryland_red, "Non-game day" = 'gray34')) +
    labs(
      title = "Tweets by day of week and time of day",
      x = "Time of Day",
      y = "# of tweets\ncreated"
    )
