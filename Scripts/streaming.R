library(rtweet)
# Please locally create Scripts/keys.r using your own Twitter account (already added to gitignore)
source("Scripts/keys.r")
get_token()

# Pull keywords from CSV for easy editing
search_terms<-read_csv("Data/keywords.csv",col_names=FALSE) %>% pull %>% paste(collapse=",")

# Set your preferred timeout
timeout <- 60 * 60 * 30
# Change the file name to something unique (but keep it in the data folder please!)
file_name <- "Data/ss_streamed_tweets"

stream_tweets(
  search_terms, 
  timeout = timeout,
  file_name = file_name,
  parse=FALSE
) 