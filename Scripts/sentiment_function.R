library(SentimentAnalysis)
library(quanteda)
library(tidyverse)
library(magrittr)

data_dictionary_emoji <- dictionary(file = "Data/emoji_dictionary.txt",format="YAML",tolower=FALSE)

# Sample use: tweets %>% bind_cols(sentimenter(.))

# By default, the function uses equation 2 on slide 11 of the 7b lecture (which is the default for analyzeSentiment)
# (positives - negatives)/total
sentimenter <- function(tweets,equation=2) {
  tweets_text <- tweets %>% select(text) %>% pull
  tweets_tokens <- tweets_text %>% corpus %>% tokens
  sentiment_table <- tweets_text %>% iconv(.,to="UTF-8") %>% analyzeSentiment(.)
  
  lexicoded <- tweets_tokens %>% tokens_lookup(dictionary = data_dictionary_LSD2015, exclusive=FALSE)
  sentiment_table %<>% bind_cols(sentiment_map(lexicoded,1)) %>% 
    mutate(SentimentLC = (lcpos-lcneg)/WordCount, NegativityLC = lcneg/WordCount, PositivityLC = lcpos/WordCount) %>% 
    select(-lcpos,-lcneg)
  
  emoji <- tweets_tokens %>% tokens_lookup(dictionary = data_dictionary_emoji, exclusive=FALSE)
  sentiment_table %<>% bind_cols(sentiment_map(lexicoded,2)) %>% 
    mutate(SentimentEmoji = (epos-eneg)/WordCount, NegativityEmoji = eneg/WordCount, PositivityEmoji = epos/WordCount) %>% 
    select(-epos,-eneg)

 
  # Use equation 1 instead
  # positives/negatives
  if (equation == 1) {
    sentiment_table %<>% mutate(SentimentGI = PositivityGI/NegativityGI,
                                SentimentHE = PositivityHE/NegativityHE,
                                SentimentLM = PositivityLM/NegativityLM,
                                SentimentQDAP = PositivityQDAP/NegativityQDAP,
                                SentimentLC = PositivityLC/NegativityLC,
                                SentimentEmoji = PositivityEmoji/NegativityEmoji)
  }
 
  # Use equation 3 instead
  # positives/(positives + negatives)
  if (equation == 3) {
    sentiment_table %<>% mutate(SentimentGI = PositivityGI/(PositivityGI+NegativityGI),
                                SentimentHE = PositivityHE/(PositivityHE+NegativityHE),
                                SentimentLM = PositivityLM/(PositivityLM+NegativityLM),
                                SentimentQDAP = PositivityQDAP/(PositivityQDAP+NegativityQDAP),
                                SentimentLC = PositivityLC/(PositivityLC+NegativityLC),
                                SentimentEmoji = PositivityEmoji/(PositivityEmoji+NegativityEmoji))
  }

  return(sentiment_table)
}

sentiment_map <- function(lookedup,use) {
  pos <- map_int(lookedup,function(x) sum(x == "POSITIVE"))
  neg <- map_int(lookedup,function(x) sum(x == "NEGATIVE"))
  negpos <- map_int(lookedup,function(x) sum(x == "NEG_POSITIVE"))
  negneg <-map_int(lookedup,function(x) sum(x == "NEG_NEGATIVE"))
  totalpos <- (pos-negpos+negneg)
  totalneg <- (neg-negneg+negpos)
  if (use == 1) {
    return(bind_cols(lcpos=totalpos,lcneg=totalneg))
  }
  else {
    return(bind_cols(epos=totalpos,eneg=totalneg))
  }
}