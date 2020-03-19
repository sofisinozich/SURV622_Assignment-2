# Reformatting emoji dictionary to be useful for our purposes
# Dictionary from Novak et al. (2015) http://hdl.handle.net/11356/1048

library(tidyverse)

emoji_sentiments <- read_csv("Data/Emoji_Sentiment_Data_v1.0.csv") %>% as_tibble(.name_repair = "universal")
emoji_sentiments %<>% mutate(Unicode.codepoint= ifelse(grepl("0x1f",Unicode.codepoint),gsub("0x","\\\\U000",Unicode.codepoint),gsub("0x","\\\\u",Unicode.codepoint)))

# Quick check due to weird display issues with backslashes
emoji_sentiments %>% filter(grepl("\\u2764",Unicode.codepoint))

# Add positive/negative evaluations
# This is basically a sentiment calculation in itself
# Will use equation 2
emoji_sentiments %<>% mutate(sentiment = (Positive-Negative)/Occurrences) 

# Some judgments on sentiment
# Anything with fewer than 100 uses is neutral (not enough data)
# Between -0.1 and 0.1 is neutral
emoji_sentiments %<>% 
  mutate(sentiment_category = case_when((sentiment > -0.1 & sentiment < 0.1) | Occurrences < 100 ~ 0,
                                        sentiment <= 0.1 ~ -1,
                                        sentiment >= 0.1 ~ 1))

# Manually created file in the correct YAML format for quanteda
data_dictionary_emoji <- dictionary(file = "Data/emoji_dictionary.txt",format="YAML",tolower=FALSE)
