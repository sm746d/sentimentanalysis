# All functions


iphoneTweetsFunction <- function(inputVal)
{
  iphoneVal <- paste("#iphone7 AND",inputVal)
  iphoneWithRetweets <- searchTwitter(iphoneVal, n=100, lang = "en")
  #remove retweets
  #iphoneTweets <-  strip_retweets(iphoneWithRetweets)
  
  #File before cleaning
  dff <- twListToDF(iphoneWithRetweets)
  write.csv(dff, file=paste('iphoneBeforeCleaning.csv'))
  
  # convert into text format
  iphoneTweets.text <- sapply(iphoneWithRetweets, function(x) x$getText())
  
  #Cleaning process
  cleanTweet <- gsub("&amp", "",iphoneTweets.text)
  cleanTweet <- gsub("@\\w+", "", cleanTweet)
  cleanTweet <- gsub("[[:punct:]]", "", cleanTweet)
  cleanTweet <- gsub("http\\w+", "", cleanTweet)
  cleanTweet <- gsub("[ \t]{2,}", "", cleanTweet)
  cleanTweet <- gsub("^\\s+|\\s+$", "", cleanTweet)
  cleanTweet <- iconv(cleanTweet, 'UTF-8', 'ASCII', sub = "")
  cleanTweet <- gsub("\r?\n|\r", " ", cleanTweet)
  #File after cleaning
  df <- as.data.frame(cleanTweet)
  write.csv(df, file=paste('iphone',inputVal,'.csv',sep = ""))
  
  
  googleVal <- paste("#googlePixel AND",inputVal)
  googleWithRetweets <- searchTwitter(googleVal, n=100, lang = "en")
  #remove retweets
  #iphoneTweets <-  strip_retweets(googleWithRetweets)
  
  #File before cleaning
  dff <- twListToDF(googleWithRetweets)
  write.csv(dff, file=paste('googleBeforeCleaning.csv'))
  
  # convert into text format
  googleTweets.text <- sapply(googleWithRetweets, function(x) x$getText())
  
  #Cleaning process
  cleanTweet <- gsub("&amp", "",googleTweets.text)
  cleanTweet <- gsub("@\\w+", "", cleanTweet)
  cleanTweet <- gsub("[[:punct:]]", "", cleanTweet)
  cleanTweet <- gsub("http\\w+", "", cleanTweet)
  cleanTweet <- gsub("[ \t]{2,}", "", cleanTweet)
  cleanTweet <- gsub("^\\s+|\\s+$", "", cleanTweet)
  cleanTweet <- iconv(cleanTweet, 'UTF-8', 'ASCII', sub = "")
  cleanTweet <- gsub("\r?\n|\r", " ", cleanTweet)
  #File after cleaning
  df <- as.data.frame(cleanTweet)
  write.csv(df, file=paste('google',inputVal,'.csv',sep = ""))
}
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- tolower(sentence)
    word.list <- strsplit(sentence, ' ')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}


sentimentCal <- function(pos.words,neg.words,fileName)
{
  Dataset <- read.csv(fileName)
  Dataset$text <- as.factor(Dataset$cleanTweet)
  
  scores <- score.sentiment(Dataset$cleanTweet, pos.words, neg.words, .progress='text')
  #print(scores$score)
  neutral <- 0
  positive <-0
  negative <- 0
  for(val in 1:length(scores$score))
  {
    if(scores[val,1]>0)
    {
      positive <- positive +1
    }
    else if(scores[val,1]<0)
    {
      negative <- negative+1
    }
    else
    {
      neutral <- neutral+1
    }
  }
  sentiment <- c("neutral","positive","negative")
  value <- c(neutral,positive,negative)
  dataFrameValue = data.frame(sentiment,value)
  #print(dataFrameValue)
  return(dataFrameValue)
}

calculateSentimentFunc <- function(fileName)
{
  Dataset <- read.csv(fileName)
  Dataset$text <- as.factor(Dataset$cleanTweet)
  pasteData <- paste(Dataset$text, sep =" ")
  #print(pasteData)
  calSentiment <- calculate_sentiment(pasteData)
  pos <- 0
  neg <- 0
  vpos <- 0
  vneg <- 0
  neu <- 0
  sar <- 0
  for(i in 1:nrow(calSentiment)) 
  {
    row <- calSentiment[i,2]
    #print(row)
    if(row == "Neutral")
    {
      neu <- neu +1
    }
    else if(row == "Positive")
    {
      pos <- pos+1
    }
    else if(row == "Negative")
    {
      neg <- neg+1
    }
    else if(row == "Very Positive")
    {
      vpos <- vpos+1
    }
    else if(row == "Very Negative")
    {
      vneg <- vneg+1
    }
    else
    {
      sar <- sar + 1
    }
  }
  # print(neu)
  # print(pos)
  # print(neg)
  # print(vpos)
  # print(vneg)
  # print(sar)
  sentiment <- c('Very Negative','Negative','Neutral','Positive','Very Positive','Sarcasm')
  value <- c(vneg,neg,neu,pos,vpos,sar)
  sentiDataFrameIphone <- data.frame(sentiment,value)
  return(sentiDataFrameIphone)
  
}


createWordCloud <- function(fileName)
{
  tweetData <- read.csv(fileName,stringsAsFactors = FALSE)
  print("-------------------------------------------------------------")
  tweetCorpus <- Corpus(VectorSource(tweetData$cleanTweet))
  print(tweetCorpus)
  tweetCorpus <- tm_map(tweetCorpus, removePunctuation)
  tweetCorpus <- tm_map(tweetCorpus, PlainTextDocument)
  tweetCorpus <- tm_map(tweetCorpus, removeWords, stopwords('english'))
  tweetCorpus <- tm_map(tweetCorpus, stemDocument)
  dtm <- TermDocumentMatrix(tweetCorpus)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(d)
  
}