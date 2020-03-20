library(SentimentAnalysis)
library(quanteda)
library(tidyverse)
library(magrittr)

data_dictionary_emoji <- dictionary(file = "Data/emoji_dictionary.txt",format="YAML",tolower=FALSE)

# Sample use: tweets %>% bind_cols(sentimenter(.))

# By default, the function uses equation 2 on slide 11 of the 7b lecture (which is the default for analyzeSentiment)
# (positives - negatives)/total
sentimenter <- function(tweets,lexicoder=FALSE, equation=2, emoji=FALSE) {
  sentiment_table <- tweets %>% select(text) %>% pull %>% analyzeSentiment(.,to='UTF-8')
  
  if (lexicoder) {
   lexicoded <- lapply(tweets$tokenized,function(x) tokens_lookup(x,dictionary = data_dictionary_LSD2015, exclusive=FALSE))
   sentiment_table %<>% bind_cols(sentiment_map(lexicoded,1))
   sentiment_table %<>% mutate(SentimentLC = (lcpos-lcneg)/WordCount, NegativityLC = lcneg/WordCount, PositivityLC = lcpos/WordCount)
  }
  
  if (emoji) {
    emojified <- lapply(tweets$tokenized, function(x) tokens_lookup(x,dictionary = data_dictionary_emoji, exclusive=FALSE))
    sentiment_table %<>% bind_cols(sentiment_map(emojified,2))
    sentiment_table %<>% mutate(SentimentEmoji = (epos-eneg)/WordCount, NegativityEmoji = eneg/WordCount, PositivityEmoji = epos/WordCount)
  }
  
  # Use equation 1 instead
  # positives/negatives
  if (equation == 1) {
    sentiment_table %<>% mutate(SentimentGI = PositivityGI/NegativityGI, 
                                SentimentHE = PositivityHE/NegativityHE,
                                SentimentLM = PositivityLM/NegativityLM,
                                SentimentQDAP = PositivityQDAP/NegativityQDAP)
    if (lexicoder) {
      sentiment_table %<>% mutate(SentimentLC = PositivityLC/NegativityLC)
    }
    if (emoji) {
      sentiment_table %<>% mutate(SentimentEmoji = PositivityEmoji/NegativityEmoji)
    }
  }
  
  # Use equation 3 instead
  # positives/(positives + negatives)
  if (equation == 3) {
    sentiment_table %<>% mutate(SentimentGI = PositivityGI/(PositivityGI+NegativityGI), 
                                SentimentHE = PositivityHE/(PositivityHE+NegativityHE),
                                SentimentLM = PositivityLM/(PositivityLM+NegativityLM),
                                SentimentQDAP = PositivityQDAP/(PositivityQDAP+NegativityQDAP))
    if (lexicoder) {
      sentiment_table %<>% mutate(SentimentLC = PositivityLC/(PositivityLC+NegativityLC))
    }
    if (emoji) {
      sentiment_table %<>% mutate(SentimentEmoji = PositivityEmoji/(PositivityEmoji+NegativityEmoji))
    }
  }
  
  return(sentiment_table)
}

sentiment_map <- function(lookedup,use) {
  pos <- map_int(lookedup,function(x) sum(x %>% pluck(1) == "POSITIVE"))
  neg <- map_int(lookedup,function(x) sum(x %>% pluck(1) == "NEGATIVE"))
  negpos <- map_int(lookedup,function(x) sum(x %>% pluck(1) == "NEG_POSITIVE"))
  negneg <-map_int(lookedup,function(x) sum(x %>% pluck(1) == "NEG_NEGATIVE"))
  totalpos <- (pos-negpos+negneg)
  totalneg <- (neg-negneg+negpos)
  if (use == 1) {
    return(bind_cols(lcpos=totalpos,lcneg=totalneg))
  }
  else {
    return(bind_cols(epos=totalpos,eneg=totalneg))
  }
}