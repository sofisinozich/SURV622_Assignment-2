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

# Tweet frequency  -------------------------------------------------------------------
  
  #_ Obtain counts of tweets by half-hour increments on key dates ----
  
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
  
  #_ Summarize tweet volume by day ----
  tweets_by_time %>%
    count(date = date(created_at_eastern)) %>%
    mutate(pct = n/sum(n))
  
  tweets_by_time %>%
    count(is_game_day) %>%
    mutate(pct = n/sum(n))
  
# Plot summary of results from streaming, with annotations for important events and gaps
  ggplot(data=relevant_tweets)+ geom_bar(aes(x=as_date(created_at_eastern)),stat="count") + 
    scale_x_date(date_breaks = "1 day", date_labels = "%b %d\n%a") +
    annotate("text",y=5000, x=as.Date("2020-03-11"), label = "← 3/8 Michigan @ Maryland") +
    annotate("text",y=3000, x=as.Date("2020-03-02"), label = "↓ 2/29 Michigan State @ Maryland") +
    annotate("text",y=1000, x=as.Date("2020-03-03"), label = "↓ No data collected ~3/2-3/4\nincludes UMD @ Rutgers", size = 3) +
    annotate("text",y=1500, x=as.Date("2020-03-12"), label = "↓ Apparent disconnect 3/11", size=3) +
    labs(x="Date",y="Relevant tweets\ncollected")

# Plot tweets by day of week and time of day
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
  
  #_ Analyze number of tweets from game days vs other days
  
  ##__ Day of Big Ten cancellation
    tweets_in_announcement_block <- tweets_by_time %>%
      filter(date == "Mar 12") %>%
      count(half_hour, name = 'Tweets', .drop = FALSE) %>%
      filter(half_hour == "11:30") %>% pull("Tweets")
    
    tweets_in_preceding_block <- tweets_by_time %>%
      filter(date == "Mar 12") %>%
      count(half_hour, name = 'Tweets', .drop = FALSE) %>%
      filter(half_hour == "11:00") %>% pull("Tweets")
  
    tweets_by_time %>%
      filter(date == "Mar 12") %>%
      count(half_hour, name = 'Tweets', .drop = FALSE) %>%
      ggplot(aes(x = half_hour, y = Tweets)) +
      geom_col(color = 'white') +
      annotate(geom = 'text', y = 95, x = "11:30", size = 3,
               label = "Big Ten Tournament\nCancellation Announced\nat 11:45 am\n↓") +
      annotate(geom = 'text', y = 120, x = "20:30",
               label = "UMD Coach Turgeon →\nappears on ESPN\nat 22:30", size = 3) +
      annotate(geom = 'text', y = 30, x = "19:00",
               label = "      Apparent\n      streaming\n      disconnect", size = 3) +
      coord_cartesian(expand = c(0,0)) +
      scale_x_discrete(name = "Time of Day (in half hour increments)",
                       labels = function(breaks) 
        case_when(endsWith(breaks, "00") ~  paste0(str_extract(breaks, ".{1,2}(?=\\:)"),
                                                   "\n",
                                                   str_extract(breaks, "(?<=\\:).{1,2}")),
                  TRUE ~ paste0("\n",
                                str_extract(breaks, "(?<=\\:).{1,2}"))
        )
      ) +
      labs(
        title = paste(c("When the Big Ten Conference announced the tournament's cancellation on March 12,",
                      "tweet activity increased around the 11:45 announcement and the 22:30 ESPN interview of Coach Turgeon."),
                      collapse = "\n")
      )
    
    ggsave(filename = "Plots/Tweet_Volume_over_Time_March12.png", plot = last_plot(),
           width = 6.5, height = 2.5, units = 'in', scale = 2,
           dpi = 620)
    
    
  # Overview plot: day of week and time of day with annotations
    tweets_by_time %>%
      count(is_game_day, date, week_index, day_of_week, half_hour, name = 'Tweets', .drop = FALSE) %>%
      mutate(date = paste0(date, "\n", day_of_week) %>% fct_reorder(as.numeric(date)),
             day_of_week = factor(day_of_week, c("Sunday", "Monday", "Tuesday", "Wednesday",
                                                 "Thursday", "Friday", "Saturday"))) %>%
      ggplot(aes(x = half_hour, y = Tweets)) +
      lemon::facet_rep_grid(week_index ~ day_of_week, repeat.tick.labels = 'x',
                            switch = 'y', drop = FALSE,
                            labeller = as_labeller(function(x) {
                              case_when(x == "Week 1" ~ "Feb. 23 - 29",
                                        x == "Week 2" ~ "Mar. 1 - 7",
                                        x == "Week 3" ~ "Mar. 8 - 14",
                                        TRUE ~ x)
                            })) +
      geom_col(aes(fill = is_game_day, color = is_game_day)) +
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
        Tweets = rep(NA_integer_, times = 11)
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
          Tweets = rep(NA_integer_)
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
          Tweets = rep(NA_integer_)
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
          Tweets = rep(NA_integer_)
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
          Tweets = rep(NA_integer_)
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
          Tweets = rep(NA_integer_)
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
        ), y = 300, label = "Michigan State\nStarts at 20:00", size = 2.75) +
        #_ Second game (Mar 8)
        geom_text(data = tibble(
          half_hour = "6:30",
          day_of_week = factor("Sunday", c("Sunday", "Monday", "Tuesday", "Wednesday",
                                           "Thursday", "Friday", "Saturday")),
          is_game_day = "Game day",
          week_index = "Week 3",
        ), y = 450, label = "Michigan\nStarts at noon", size = 2.75) +
        #_ Tournament cancellation
        geom_text(data = tibble(
          half_hour = "11:30",
          day_of_week = factor("Thursday", c("Sunday", "Monday", "Tuesday", "Wednesday",
                                             "Thursday", "Friday", "Saturday")),
          is_game_day = "Non-game day",
          week_index = "Week 3",
        ), y = 300, label = "Big Ten\nTournament\nCancelled\n↓", size = 2.75) +
      coord_cartesian(clip = 'off') +
      # Add scales and titles
      scale_x_discrete(name = "Time of Day (in half hour increments)",
                       breaks = levels(tweets_by_time$half_hour),
                       labels = function(breaks) ifelse(endsWith(breaks, ":00") &
                                                        str_detect(breaks, "^(0|8|12|16|20)"),
                                                        str_extract(breaks, ".{1,2}(?=\\:)"),
                                                        "")) +
      scale_fill_manual(name = "Type of day",
                        values = c("Game day" = .maryland_red, "Non-game day" = 'gray34')) +
      scale_color_manual(name = "Type of day",
                        values = c("Game day" = .maryland_red, "Non-game day" = 'gray34')) +
      labs(
        subtitle = "Number of tweets by day and time of day",
        title = "The volume of tweets is vastly larger on game days.\nEven on the day of the Big Ten Tournament's cancellation, the volume of tweets is nowhere as large as on a game day."
      )
    
    ggsave(filename = "Plots/Tweet_Volume_over_Time_Entire_Period.png", plot = last_plot(),
           width = 6.5, height = 3.25, units = 'in', scale = 2,
           dpi = 500)
