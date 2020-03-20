# Irrelevant (IF "Not Basketball" irrelevant words AND NOT "basketball | bball | hoops" OR (IF "Not Men's" irrelevant words AND NOT "men | men's | mbb")

irrelevant2 <- read_csv("Data/irrelevant2.csv",col_names=TRUE)

relevantMenWords = c("men", "men's", "mbb")
irrelevantMenWords = irrelevant2$MEN
relevantSportWords = c("basketball", "bball", "hoops")
irrelevantSportWords = na.omit(irrelevant2$BASKETBALL)
newTweets = data.frame()
irrelevantTweets = data.frame()

for(i in 1:nrow(relevant_tweets)){
  # break up tweet into indiviaul words
  tweet = relevant_tweets$text[i]
  tweet = tolower(tweet)
  words = tokens(tweet)
  # if it contains an irrelevant word, put it in the irrelevant data frame
  if((length(intersect(words$text1, irrelevantMenWords))>0 & 
      length(intersect(words$text1, relevantMenWords))<1) |
     (length(intersect(words$text1, irrelevantSportWords))>0 & 
      length(intersect(words$text1, relevantSportWords))<1)){
    irrelevantTweets = rbind(irrelevantTweets, relevant_tweets[i,])
  }
  else{
    newTweets = rbind(newTweets, relevant_tweets[i,])
  }
}
