library(rtweet)
library(qdap)
library(tidyverse)
library(magrittr)
oneday_tweets <- parse_stream("Data/ss_streamed_tweets.json")
oneday_tweets %>% select(text)
oneday_scrubbed <- oneday_tweets$text %>% scrubber() %sw% qdapDictionaries::Top200Words

link_regex <- "https : / / t. co / [a-z0-9]{10}"
oneday_scrubbed %<>% gsub(link_regex,"",.)

freq_terms(oneday_scrubbed)
