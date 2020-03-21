library(SentimentAnalysis)
library(quanteda)
library(tidyverse)
library(magrittr)

data_dictionary_emoji <- dictionary(file = "Data/emoji_dictionary.txt",format="YAML",tolower=FALSE)
data_dictionary_lcemoji <- dictionary(c(as.list(data_dictionary_emoji),as.list(data_dictionary_LSD2015)))

# Sample use: tweets %>% bind_cols(sentimenter(.))

# By default, the function uses equation 2 on slide 11 of the 7b lecture (which is the default for analyzeSentiment)
# (positives - negatives)/total
sentimenter <- function(tweets,equation=2) {
  tweets_text <- tweets %>% select(text) %>% pull
  tweets_tokens <- tweets_text %>% corpus %>% 
    tokens(remove_punct = TRUE,remove_url=TRUE,remove_separators=TRUE) %>% 
    tokens_remove(stopwords("english")) %>% # Remove English stopwords
    tokens_remove("^[A-z]{1}$",valuetype="regex") # Remove single-letter words
  tokens_length <- tweets_tokens %>% map_int(.,function(x) length(x))
  sentiment_table <- tweets_text %>% iconv(.,to="UTF-8") %>% analyzeSentiment(.) %>% mutate(TokensCount = tokens_length)
  
  lexicoded <- tweets_tokens %>% tokens_lookup(dictionary = data_dictionary_LSD2015, exclusive=FALSE)
  sentiment_table %<>% bind_cols(sentiment_map(lexicoded,1)) %>% 
    mutate(SentimentLC = (lcpos-lcneg)/tokens_length, NegativityLC = lcneg/tokens_length, PositivityLC = lcpos/tokens_length) %>% 
    select(-lcpos,-lcneg)
  
  emoji <- tweets_tokens %>% tokens_lookup(dictionary = data_dictionary_emoji, exclusive=FALSE)
  sentiment_table %<>% bind_cols(sentiment_map(emoji,2)) %>% 
    mutate(SentimentEmoji = (epos-eneg)/tokens_length, NegativityEmoji = eneg/tokens_length, PositivityEmoji = epos/tokens_length) %>% 
    select(-epos,-eneg)
  
  lexicoded_emoji <- tweets_tokens %>% tokens_lookup(dictionary = data_dictionary_lcemoji,exclusive=FALSE)
  sentiment_table %<>% bind_cols(sentiment_map(lexicoded_emoji,3)) %>%
    mutate(SentimentLCE = (lcepos-lceneg)/tokens_length, NegativityLCE = lceneg/tokens_length, PositivityLCE = lcepos/tokens_length) %>% 
    select(-lcepos,-lceneg)
    

 
  # Use equation 1 instead
  # positives/negatives
  if (equation == 1) {
    sentiment_table %<>% mutate(SentimentGI = PositivityGI/NegativityGI,
                                SentimentHE = PositivityHE/NegativityHE,
                                SentimentLM = PositivityLM/NegativityLM,
                                SentimentQDAP = PositivityQDAP/NegativityQDAP,
                                SentimentLC = PositivityLC/NegativityLC,
                                SentimentEmoji = PositivityEmoji/NegativityEmoji,
                                SentimentLCE = PositivityLCE/NegativityLCE)
  }
 
  # Use equation 3 instead
  # positives/(positives + negatives)
  if (equation == 3) {
    sentiment_table %<>% mutate(SentimentGI = PositivityGI/(PositivityGI+NegativityGI),
                                SentimentHE = PositivityHE/(PositivityHE+NegativityHE),
                                SentimentLM = PositivityLM/(PositivityLM+NegativityLM),
                                SentimentQDAP = PositivityQDAP/(PositivityQDAP+NegativityQDAP),
                                SentimentLC = PositivityLC/(PositivityLC+NegativityLC),
                                SentimentEmoji = PositivityEmoji/(PositivityEmoji+NegativityEmoji),
                                SentimentLCE = PositivityLCE/(PositivityLCE+NegativityLCE))
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
  else if (use == 2) {
    return(bind_cols(epos=totalpos,eneg=totalneg))
  }
  else if (use == 3) {
    return(bind_cols(lcepos=totalpos,lceneg=totalneg))
  }
}