# Load data, packages, and any necessary functions

library(rtweet)
library(quanteda)
library(qdap)

library(tidyverse)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(magrittr)
library(lemon)

  source("Scripts/load_dataviz_themes.R")
  
  #_ Load tweets with sentiment classifications
    relevant_tweets_sentiment <- read_rds("Data/tokenized_relevant_tweets_sentiment.rds")
  
  #_ Import a table of events that can be used for contextualizing analysis
  #_ (e.g. timing of press release cancelling the Big Ten tournament)
  contextual_events <- read_csv("data/contextual-events-timeline.csv") %>%
    mutate(Time = ymd_hms(Time, tz = "America/New_York"))
  
# Group data by meaningful units of analysis across time ----
  tweets_by_time <- relevant_tweets_sentiment %>%
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
  
# Plot raw tweet sentiment over time ----
  tweets_by_time %>%
    mutate(date = paste0(date, "\n", day_of_week) %>% fct_reorder(as.numeric(date)),
           day_of_week = factor(day_of_week, c("Sunday", "Monday", "Tuesday", "Wednesday",
                                               "Thursday", "Friday", "Saturday"))) %>%
    group_by(is_game_day, date, week_index, day_of_week, half_hour, name = 'Tweets', .drop = FALSE) %>%
    summarize(average_sentiment = mean(SentimentGI, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!is.na(week_index), !is.na(day_of_week)) %>%
    mutate(sentiment_sign = case_when(average_sentiment > 0 ~ 'Positive',
                                      average_sentiment < 0 ~ 'Negative',
                                      TRUE ~ NA_character_)) %>%
    ggplot(aes(x = half_hour, y = average_sentiment)) +
    lemon::facet_rep_grid(week_index ~ day_of_week, repeat.tick.labels = 'x',
                          switch = 'y', drop = FALSE,
                          labeller = as_labeller(function(x) {
                            case_when(x == "Week 1" ~ "Feb. 23 - 29",
                                      x == "Week 2" ~ "Mar. 1 - 7",
                                      x == "Week 3" ~ "Mar. 8 - 14",
                                      TRUE ~ x)
                          })) +
    geom_point(aes(color = sentiment_sign)) +
    # Add horizontal line for 0
    geom_hline(yintercept = 0, linetype = 2) +
    # Grey out entire days without data collection
    geom_rect(data = tibble(
      day_of_week = factor(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", 
                             "Monday", "Tuesday", "Wednesday", "Wednesday", "Saturday"),
                           c("Sunday", "Monday", "Tuesday", "Wednesday",
                             "Thursday", "Friday", "Saturday")),
      is_game_day = c("Game day", "Non-game day", "Non-game day", "Game day", "Non-game day", "Non-game day",
                      "Non-game day", "Game day", "Non-game day", "Non-game day", "Non-game day"),
      week_index = c("Week 1", "Week 1", "Week 1", "Week 1", "Week 1", "Week 1",
                     "Week 2", "Week 2", "Week 2", "Week 3", "Week 3"),
      half_hour = factor(rep("0:00", times = 11),
                         levels(tweets_by_time$half_hour)),
      average_sentiment = rep(NA_real_, times = 11)
    ), xmin = factor("0:00", levels(tweets_by_time$half_hour)), xmax = factor("23:30", levels(tweets_by_time$half_hour)),
    ymin = -Inf, ymax = Inf,
    fill = 'lightgray', alpha = 0.75) +
    # Grey out specific time ranges with streaming disconnects or other non-coverage
    #_ February 29: 0:00 through 16:30
    geom_rect(data = tibble(
      day_of_week = factor(c("Saturday"),
                           c("Sunday", "Monday", "Tuesday", "Wednesday",
                             "Thursday", "Friday", "Saturday")),
      is_game_day = c("Game day"),
      week_index = c("Week 1"),
      half_hour = factor(rep("0:00", times = 11),
                         levels(tweets_by_time$half_hour)),
      average_sentiment = rep(NA_real_)
    ),
    xmin = factor("0:00", levels(tweets_by_time$half_hour)),
    xmax = factor("16:30", levels(tweets_by_time$half_hour)),
    ymin = -Inf, ymax = Inf,
    fill = 'lightgray', alpha = 0.75) +
    #_ March 5: 0:00 through 9:52
    geom_rect(data = tibble(
      day_of_week = factor(c("Thursday"),
                           c("Sunday", "Monday", "Tuesday", "Wednesday",
                             "Thursday", "Friday", "Saturday")),
      is_game_day = c("Non-game day"),
      week_index = c("Week 2"),
      half_hour = factor(rep("0:00", times = 1),
                         levels(tweets_by_time$half_hour)),
      average_sentiment = rep(NA_real_)
    ),
    xmin = factor("0:00", levels(tweets_by_time$half_hour)),
    xmax = factor("9:00", levels(tweets_by_time$half_hour)),
    ymin = -Inf, ymax = Inf,
    fill = 'lightgray', alpha = 0.75) +
    #_ March 9: 9:00 through 22:30
    geom_rect(data = tibble(
      day_of_week = factor(c("Monday"),
                           c("Sunday", "Monday", "Tuesday", "Wednesday",
                             "Thursday", "Friday", "Saturday")),
      is_game_day = c("Non-game day"),
      week_index = c("Week 3"),
      half_hour = factor(rep("0:00", times = 11),
                         levels(tweets_by_time$half_hour)),
      average_sentiment = rep(NA_real_)
    ),
    xmin = factor("9:00", levels(tweets_by_time$half_hour)),
    xmax = factor("22:30", levels(tweets_by_time$half_hour)),
    ymin = -Inf, ymax = Inf,
    fill = 'lightgray', alpha = 0.75) +
    #_ March 12: 18:30 through 20:00
    geom_rect(data = tibble(
      day_of_week = factor(c("Thursday"),
                           c("Sunday", "Monday", "Tuesday", "Wednesday",
                             "Thursday", "Friday", "Saturday")),
      is_game_day = c("Non-game day"),
      week_index = c("Week 3"),
      half_hour = factor(rep("0:00", times = 11),
                         levels(tweets_by_time$half_hour)),
      average_sentiment = rep(NA_real_)
    ),
    xmin = factor("18:30", levels(tweets_by_time$half_hour)),
    xmax = factor("20:00", levels(tweets_by_time$half_hour)),
    ymin = -Inf, ymax = Inf,
    fill = 'lightgray', alpha = 0.75) +
    #_ March 13: 13:00 through 23:30
    geom_rect(data = tibble(
      day_of_week = factor(c("Friday"),
                           c("Sunday", "Monday", "Tuesday", "Wednesday",
                             "Thursday", "Friday", "Saturday")),
      is_game_day = c("Non-game day"),
      week_index = c("Week 3"),
      half_hour = factor(rep("0:00", times = 11),
                         levels(tweets_by_time$half_hour)),
      average_sentiment = rep(NA_real_)
    ),
    xmin = factor("13:00", levels(tweets_by_time$half_hour)),
    xmax = factor("23:30", levels(tweets_by_time$half_hour)),
    ymin = -Inf, ymax = Inf,
    fill = 'lightgray', alpha = 0.75) +
    # Text annotations
    #_ First game (Feb 29)
    geom_text(data = tibble(
      half_hour = "12:00",
      day_of_week = factor("Saturday", c("Sunday", "Monday", "Tuesday", "Wednesday",
                                         "Thursday", "Friday", "Saturday")),
      is_game_day = "Game day",
      week_index = "Week 1",
    ), y = 0.3, label = "Michigan State\nStarts at 20:00", size = 2.75) +
    #_ Second game (Mar 8)
    geom_text(data = tibble(
      half_hour = "6:30",
      day_of_week = factor("Sunday", c("Sunday", "Monday", "Tuesday", "Wednesday",
                                       "Thursday", "Friday", "Saturday")),
      is_game_day = "Game day",
      week_index = "Week 3",
    ), y = 0.3, label = "Michigan\nStarts at noon", size = 2.75) +
    #_ Tournament cancellation
    geom_text(data = tibble(
      half_hour = "11:30",
      day_of_week = factor("Thursday", c("Sunday", "Monday", "Tuesday", "Wednesday",
                                         "Thursday", "Friday", "Saturday")),
      is_game_day = "Non-game day",
      week_index = "Week 3",
    ), y = 0.3, label = "Big Ten\nTournament\nCancelled\n↓", size = 2.5) +
    # Add scales and titles
    scale_x_discrete(name = "Time of Day (in half hour increments)",
                     breaks = levels(tweets_by_time$half_hour),
                     labels = function(breaks) ifelse(endsWith(breaks, ":00") &
                                                        str_detect(breaks, "^(0|8|12|16|20)"),
                                                      str_extract(breaks, ".{1,2}(?=\\:)"),
                                                      "")) +
    scale_color_manual(name = "Average tweet's sentiment:", values = c("Positive" = 'dodgerblue4',
                                                                       "Negative" = 'orange2'),
                       na.translate = FALSE) +
    scale_y_continuous(name = "**Average Tweet Sentiment**<br>*GI Dictionary<br>(Positives-Negatives)/Total*",
                       breaks = seq(-0.4, 0.4, 0.1), labels = seq(-0.4, 0.4, 0.1),
                       limits = c(-0.4, 0.4)) +
    coord_cartesian(clip = 'off') +
    labs(
      title = "Over the period of streaming, sentiment was generally more positive than negative,\neven when Maryland lost to Michigan State and after the Big Ten tournament was cancelled."
    ) +
    theme(
      axis.title.y = ggtext::element_markdown(family = 'Arial Narrow', face = 'plain',
                                              size = 12, angle = 0, vjust = 0.5)
    )
  
  ggsave(filename = "Plots/Sentiment_Over_Time_Entire_Period.png", plot = last_plot(),
         width = 6.5, height = 3, units = 'in', scale = 2,
         dpi = 550)
  
# Highlight certain days ----
  #_ March 12, Big Ten Cancellation
  
  tweets_by_time %>%
    filter(date == "Mar 12") %>%
    group_by(half_hour, .drop = FALSE) %>%
    summarize(average_sentiment = mean(SentimentGI, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(sentiment_sign = case_when(average_sentiment > 0 ~ 'Positive',
                                      average_sentiment < 0 ~ 'Negative',
                                      TRUE ~ NA_character_)) %>%
    ggplot(aes(x = half_hour, y = average_sentiment)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_col(aes(fill = sentiment_sign), color = 'white') +
    annotate(geom = 'text', y = 0.1, x = "11:30", size = 2.8,
             label = "  Big Ten Tournament\n  Cancellation Announced\n  at 11:45 am\n  ↓") +
    annotate(geom = 'text', y = 0.175, x = "20:30",
             label = "UMD Coach Turgeon →\nappears on ESPN\nat 22:30", size = 2.8) +
    annotate(geom = 'text', y = 0.03, x = "19:00",
             label = "      Apparent\n      streaming\n      disconnect", size = 2.8) +
    coord_cartesian(expand = c(0,0), clip = 'off') +
    scale_y_continuous(name = "**Average Tweet Sentiment**<br>*GI Dictionary<br>(Positives-Negatives)/Total*",
                       breaks = seq(-0.2, 0.2, 0.1), labels = seq(-0.2, 0.2, 0.1),
                       limits = c(-0.2, 0.2)) +
    scale_x_discrete(name = "Time of Day (in half hour increments)",
                     labels = function(breaks) 
                       case_when(endsWith(breaks, "00") ~  paste0(str_extract(breaks, ".{1,2}(?=\\:)"),
                                                                  "\n",
                                                                  str_extract(breaks, "(?<=\\:).{1,2}")),
                                 TRUE ~ paste0("\n",
                                               str_extract(breaks, "(?<=\\:).{1,2}"))
                       )
    ) +
    scale_fill_manual(name = "Average tweet's sentiment:", values = c("Positive" = 'dodgerblue4',
                                 "Negative" = 'orange2')) +
    labs(
      title = paste(c("On the day that the Big Ten Conference announced the tournament's cancellation, sentiment peaked in the period surrounding Coach Turgeon's ESPN interview.",
                      "Sentiment was generally more positive than negative, apart from a handful of early-morning periods with a small number of tweets."),
                    collapse = "\n")
    ) +
    theme(
      axis.title.y = ggtext::element_markdown(family = 'Arial Narrow', face = 'plain',
                                              size = 12, angle = 0, vjust = 0.5)
    )
  
  ggsave(filename = "Plots/Sentiment_Over_Time_March12.png", plot = last_plot(),
         width = 6.5, height = 2.5, units = 'in', scale = 2,
         dpi = 620)
  
    
  
  tweets_by_time %>%
    filter(date == "Mar 8") %>%
    group_by(half_hour, .drop = FALSE) %>%
    summarize(average_sentiment = mean(SentimentGI, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(sentiment_sign = case_when(average_sentiment > 0 ~ 'Positive',
                                      average_sentiment < 0 ~ 'Negative',
                                      TRUE ~ NA_character_) %>%
             factor(c("Positive", "Negative"))) %>%
    ggplot(aes(x = half_hour, y = average_sentiment)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_col(aes(fill = sentiment_sign), color = 'white') +
    annotate(geom = 'text', y = 0.1, x = "12:00", size = 2.8,
             label = "  Game against Michigan begins  \n  at 12:00\n  ↓") +
    annotate(geom = 'text', y = 0.125, x = "14:30",
             label = "UMD wins\nagainst Michigan\nat ~ 14:15", size = 2.8) +
    coord_cartesian(expand = c(0,0), clip = 'off') +
    scale_y_continuous(name = "**Average Tweet Sentiment**<br>*GI Dictionary<br>(Positives-Negatives)/Total*",
                       breaks = seq(-0.2, 0.2, 0.1), labels = seq(-0.2, 0.2, 0.1),
                       limits = c(-0.2, 0.2)) +
    scale_x_discrete(name = "Time of Day (in half hour increments)",
                     labels = function(breaks) 
                       case_when(endsWith(breaks, "00") ~  paste0(str_extract(breaks, ".{1,2}(?=\\:)"),
                                                                  "\n",
                                                                  str_extract(breaks, "(?<=\\:).{1,2}")),
                                 TRUE ~ paste0("\n",
                                               str_extract(breaks, "(?<=\\:).{1,2}"))
                       )
    ) +
    scale_fill_manual(name = "Average tweet's sentiment:", values = c("Positive" = 'dodgerblue4',
                                                                      "Negative" = 'orange2'),
                      guide = FALSE) +
    labs(
      title = paste(c("When Maryland defeated Michigan on March 8, sentiment was generally positive throughout the day but markedly increased towards the end of the game."),
                    collapse = "\n")
    ) +
    theme(
      axis.title.y = ggtext::element_markdown(family = 'Arial Narrow', face = 'plain',
                                              size = 12, angle = 0, vjust = 0.5)
    )
  
  ggsave(filename = "Plots/Sentiment_Over_Time_March8.png", plot = last_plot(),
         width = 6.5, height = 2.5, units = 'in', scale = 2,
         dpi = 620)
  