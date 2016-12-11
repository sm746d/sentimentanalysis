#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(twitteR)
library(httr)
library(devtools)
library(ROAuth)
library(stringr)
library(shiny)
library(e1071)
library(plyr)
#library(dplyr)
#library(NLP)
library(SnowballC)
#library(RWeka)
library(RColorBrewer)
#library(sentimentr)
library(RSentiment)
library(reshape2)
library(ggmap)
library(tm)
library(wordcloud)

source("otherScripts/function.R")

consumerKey <- "u9hajvgH1xJOiVHxQIgdcecZJ"
consumerSecret <- "WpigB2NitSdAH6OajWsNFC91JbKMoTpe0KjD5nEmlY38QkIUB7"
accessToken <- "3070141472-6MWRiw22WfuVu5Hbdu0l6BkwU8HPYtmAV8vqtuC"
accessSecret <- "3LE29pXpaObeo4inAjU6Jjho92pw1P0sSeXEKJ4DH4iSV"
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessSecret)

shinyServer(function(input, output) 
{
  # bar graph
  output$iGraphBar1 <- renderPlot({
    print(input$algo)
    readAllInputValue(input$searchKeyword)
    if(input$algo==1)
    {
      iphoneFrame <- sentimentCal(pos.words,neg.words,paste('iphone',str_to_lower(str_trim(input$searchKeyword)),'.csv',sep = ""))
      ggplot(iphoneFrame,aes(y=value,x=sentiment))+geom_bar(stat="identity",color="green", aes(fill=sentiment),position =
                                                              'dodge',width = 1)+scale_fill_brewer(palette="Dark2")+ylim(0, 100)
      
      }
    else
    {
      sentiDframe <- calculateSentimentFunc(paste('iphone',str_to_lower(str_trim(input$searchKeyword)),'.csv',sep = ""))
      print(sentiDframe)
      ggplot(sentiDframe,aes(y=value,x=sentiment))+geom_bar(stat="identity",color="green", aes(fill=sentiment),position =
                                                              'dodge',width = 1)+scale_fill_brewer(palette="RdGy")+ylim(0, 100)
    }
    
  })
  
  output$iGraphBar2 <- renderPlot({
    if(input$algo==1)
    {
      googleFrame <- sentimentCal(pos.words,neg.words,paste('google',str_to_lower(str_trim(input$searchKeyword)),'.csv',sep = ""))
      ggplot(googleFrame,aes(y=value,x=sentiment))+geom_bar(stat="identity",color="green", aes(fill=sentiment),position =
                                                              'dodge',width = 1)+scale_fill_brewer(palette="Dark2")+ylim(0, 100)
    }
    else
    {
      sentiDframe <- calculateSentimentFunc(paste('google',str_to_lower(str_trim(input$searchKeyword)),'.csv',sep = ""))
      print(sentiDframe)
      ggplot(sentiDframe,aes(y=value,x=sentiment))+geom_bar(stat="identity",color="green", aes(fill=sentiment),position =
                                                              'dodge',width = 1)+scale_fill_brewer(palette="RdGy")+ylim(0, 100)
    }
  })
  #bar graph
  
  
  # #pie chart
  output$iGraphPie1 <- renderPlot({
    print(input$algo)
    readAllInputValue(input$searchKeyword)
    if(input$algo==1)
    {
      iphoneFrame <- sentimentCal(pos.words,neg.words,paste('iphone',str_to_lower(str_trim(input$searchKeyword)),'.csv',sep = ""))
      ggplot(iphoneFrame,aes(y=value,x=sentiment))+geom_bar(stat="identity",color="green", aes(fill=sentiment),position ='dodge',width = 1)+scale_fill_brewer(palette="Dark2")+coord_polar()+theme_bw()+ylim(0, 100)
    }
    else
    {
      sentiDframe <- calculateSentimentFunc(paste('iphone',str_to_lower(str_trim(input$searchKeyword)),'.csv',sep = ""))
      print(sentiDframe)
      ggplot(sentiDframe,aes(y=value,x=sentiment))+geom_bar(stat="identity",color="green", aes(fill=sentiment),position = 'dodge')+scale_fill_brewer(palette="RdGy")+coord_polar(theta = "y")+theme_bw()+ylim(0, 100)
    }
    
  })
  
  output$iGraphPie2 <- renderPlot({
    if(input$algo==1)
    {
      googleFrame <- sentimentCal(pos.words,neg.words,paste('google',str_to_lower(str_trim(input$searchKeyword)),'.csv',sep = ""))
      ggplot(googleFrame,aes(y=value,x=sentiment))+geom_bar(stat="identity",color="green", aes(fill=sentiment),position =
                                                              'dodge',width = 1)+scale_fill_brewer(palette="Dark2")+coord_polar()+theme_bw()+ylim(0, 100)
    }
    else
    {
      sentiDframe <- calculateSentimentFunc(paste('google',str_to_lower(str_trim(input$searchKeyword)),'.csv',sep = ""))
      print(sentiDframe)
      ggplot(sentiDframe,aes(y=value,x=sentiment))+geom_bar(stat="identity",color="green", aes(fill=sentiment),position = 'dodge')+scale_fill_brewer(palette="RdGy")+coord_polar(theta = "y")+theme_bw()+ylim(0, 100)
    }
  })
  
  # #pie chart
  
  # #line graph
  
  output$iGraphLine1 <- renderPlot({
    print(input$algo)
    readAllInputValue(input$searchKeyword)
    if(input$algo==1)
    {
      iphoneFrame <- sentimentCal(pos.words,neg.words,paste('iphone',str_to_lower(str_trim(input$searchKeyword)),'.csv',sep = ""))
      ggplot(iphoneFrame, aes(x=sentiment, y=value, group=1, colour=sentiment)) +
        geom_line() +
        geom_point()
    }
    else
    {
      sentiDframe <- calculateSentimentFunc(paste('iphone',str_to_lower(str_trim(input$searchKeyword)),'.csv',sep = ""))
      print(sentiDframe)
      ggplot(sentiDframe, aes(x=sentiment, y=value, group=1, colour=sentiment)) +
        geom_line() +
        geom_point()
    }
    
  })
  
  output$iGraphLine2 <- renderPlot({
    if(input$algo==1)
    {
      googleFrame <- sentimentCal(pos.words,neg.words,paste('google',str_to_lower(str_trim(input$searchKeyword)),'.csv',sep = ""))
      ggplot(googleFrame, aes(x=sentiment, y=value, group=1, colour=sentiment)) +
        geom_line() +
        geom_point()
    }
    else
    {
      sentiDframe <- calculateSentimentFunc(paste('google',str_to_lower(str_trim(input$searchKeyword)),'.csv',sep = ""))
      print(sentiDframe)
      ggplot(sentiDframe, aes(x=sentiment, y=value, group=1, colour=sentiment)) +
        geom_line() +
        geom_point()
    }
  })
  # #line graph
  
  #Word Cloud
  
  output$iGraphcloud <- renderPlot({
    tweetCloud <- createWordCloud(paste('iphone',str_to_lower(str_trim(input$searchKeyword)),'.csv',sep = ""))
    wordcloud(words = tweetCloud$word, freq = tweetCloud$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
  })
  output$gGraphcloud <- renderPlot({
    tweetCloud <- createWordCloud(paste('google',str_to_lower(str_trim(input$searchKeyword)),'.csv',sep = ""))
    wordcloud(words = tweetCloud$word, freq = tweetCloud$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  #word Cloud
  
})

readAllInputValue <- function(inputVal)
{
  iphoneTweetsFunction(str_to_lower(str_trim(inputVal)))
  
}

pos <- scan('C:/Users/Shaan/R/mobilecomparison/wordList/positive.txt', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('C:/Users/Shaan/R/mobilecomparison/wordList/negative.txt', what='character', comment.char=';') #folder with negative dictionary
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')
#print(length(pos.words))
#print(length(neg.words))







