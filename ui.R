#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Mobile Comparison from Twitter Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(position = "left",
                sidebarPanel(
                  #sliderInput("bins","Number of bins:",min = 1,max = 50,value = 30)
                  ("Enter the search keyword"),
                  textInput("searchKeyword","Search value","camera"),
                  radioButtons("algo","select the algorithm",list("Jeffery Breen Concept"="1","RSentiment Package"="2"),"1"),
                  submitButton("Find Sentiment")
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  ("Output Pannel"),
                  tabsetPanel(
                    tabPanel("Bar Chart", plotOutput("iGraphBar1"),plotOutput("iGraphBar2")), 
                    tabPanel("Pie Chart", plotOutput("iGraphPie1"),plotOutput("iGraphPie2")), 
                    tabPanel("Line Chart", plotOutput("iGraphLine1"),plotOutput("iGraphLine2")),
                    tabPanel("Word Cloud", plotOutput("iGraphcloud"),plotOutput("gGraphcloud"))
                  )
                  # textOutput("searchValue"),
                  # textOutput("genderValue"),
                  # plotOutput("iGraph"),
                  # plotOutput("iGraph2")
                  
                )
  )
))
