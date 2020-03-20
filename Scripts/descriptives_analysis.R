#Install qdap
install.packages(qdap)

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
frequent_terms = freq_terms(relevant_tweets["text"], 30)
plot(frequent_terms)

# Import a table of events that can be used for contextualizing analysis
# (e.g. timing of press release cancelling the Big Ten tournament)
  contextual_events <- read_csv("data/contextual-events-timeline.csv") %>%
    mutate(Time = ymd_hms(Time, tz = "America/New_York"))

# Plots -------------------------------------------------------------------

# Summary of results from streaming, with annotations for important events and gaps
  
  ggplot(data=relevant_tweets) + 
    geom_bar(aes(x = as_date(created_at_eastern)), stat="count") + 
    scale_x_date(date_breaks = "1 day", date_labels = "%b %d\n%a") +
    annotate("text",y=5000, x=as.Date("2020-03-11"), label = "← 3/8 Michigan @ Maryland") +
    annotate("text",y=3500, x=as.Date("2020-02-29"), label = "2/29\nMichigan State\n@\nMaryland\n↓", size = 3) +
    annotate("text",y=1000, x=as.Date("2020-03-03"), label = "No data collected ~3/2-3/4\nincludes UMD @ Rutgers", size = 3) +
    annotate("text",y=1100, x=as.Date("2020-03-11"), label = "Apparent\ndisconnect\n3/11\n↓", size=3.5) +
    annotate("text",y=2000, x=as.Date("2020-03-12"), label = "Big Ten\nTournament\nCancelled\n↓", size=3.5) +
    labs(x= "Date", y = "Relevant tweets\ncollected",
         title = "")

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
  
  #_ Tweets by half-hour increments on key dates ----
  
    tweets_by_time <- relevant_tweets %>%
      mutate(
        week_index = case_when(
          month(created_at_eastern) == 2 ~ "Week 1",
          month(created_at_eastern) == 3 & day(created_at_eastern) <= 7 ~ "Week 2",
          TRUE ~ "Week 3"
        ),
        date = paste0(month(created_at_eastern, label = TRUE), " ", day(created_at_eastern)),
        day_of_week = wday(created_at_eastern, label = TRUE, abbr = FALSE) %>%
          as.character(),
        is_game_day = case_when(
          as.character(date(created_at_eastern)) %in% c("2020-02-29", "2020-03-03", "2020-03-08") ~ "Game day",
          TRUE ~ "Non-game day"
        ),
        half_hour = paste0(hour(created_at_eastern),
                           ifelse(minute(created_at_eastern) < 30, ":00", ":30")
        ) %>%
          factor(levels = map(0:23, ~ paste0(.x, c(":00", ":30"))) %>% unlist())
      )
  
  ##__ Day of Big Ten cancellation
  tweets_by_time %>%
    filter(date == "Mar 12") %>%
    count(half_hour, name = 'Tweets', .drop = FALSE) %>%
    ggplot(aes(x = half_hour, y = Tweets)) +
    geom_col(aes()) +
    coord_cartesian(expand = c(0,0)) +
    scale_x_discrete(name = "Time of Day (in half hour increments)",
                     labels = function(breaks) 
      case_when(endsWith(breaks, "00") ~  paste0(str_extract(breaks, ".{1,2}(?=\\:)"),
                                                 "\n",
                                                 str_extract(breaks, "(?<=\\:).{1,2}")),
                TRUE ~ paste0("\n",
                              str_extract(breaks, "(?<=\\:).{1,2}"))
      )
    )
    
  tweets_by_time %>%
    count(is_game_day, date, week_index, day_of_week, half_hour, name = 'Tweets', .drop = FALSE) %>%
    mutate(date = paste0(date, "\n", day_of_week) %>% fct_reorder(as.numeric(date)),
           day_of_week = factor(day_of_week, c("Sunday", "Monday", "Tuesday", "Wednesday",
                                               "Thursday", "Friday", "Saturday"))) %>%
    ggplot(aes(x = half_hour, y = Tweets)) +
    geom_col(aes(fill = is_game_day, color = is_game_day)) +
    lemon::facet_rep_grid(week_index ~ day_of_week, repeat.tick.labels = 'x',
                          switch = 'y', drop = FALSE) +
    scale_x_discrete(name = "Time of Day (in half hour increments)",
                     labels = function(breaks) ifelse(endsWith(breaks, ":00") &
                                                      str_detect(breaks, "^(0|8|12|16|20)"),
                                                      str_extract(breaks, ".{1,2}(?=\\:)"),
                                                      "")) +
    scale_fill_manual(name = "Type of day",
                      values = c("Game day" = .maryland_red, "Non-game day" = 'gray34')) +
    scale_color_manual(name = "Type of day",
                      values = c("Game day" = .maryland_red, "Non-game day" = 'gray34'))
