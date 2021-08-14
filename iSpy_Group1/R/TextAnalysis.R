# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyverse)
library(plotly)
library(tools)
library(lubridate)
library(stringi)
library(clock)

# Load data
raw_text <- read_csv("H:/jovina7/BizMakeover2/_posts/shinny-mc3/clean_data.csv")
blog <- filter(raw_text,type=='mbdata')
call <- filter(raw_text,type=='ccdata') 

# Define UI for application
exploreUI <- function(id){
  fluidPage(
  fluidRow(
    column(4,pickerInput(NS(id,'datatype'),label='Datatype',
          choices =c('mbdata' = 'mbdata',
                     'ccdata' = 'ccdata'),
          selected = "mbdata",
          options = list(`actions-box` = TRUE),
          multiple = T)),
    column(4,pickerInput(NS(id,'timeperiod'),label='Timeperiod',
                         choices =c('5pm' = '5pm',
                                    '6pm' = '6pm',
                                    '7pm' = '7pm',
                                    '8pm' = '8pm',
                                    '9pm' = '9pm'),
                         selected = "5pm",
                         options = list(`actions-box` = TRUE),
                         multiple = T))),
  fluidRow(
    column(6,plotlyOutput(NS(id,"wordcloud"), width = "1000px", height = "700px")),
    column(6,plotlyOutput(NS(id,"tfitf"),width = "900px", height = "700px")),
    column(6,plotlyOutput(NS(id,"topicdistribution"), width = "1000px", height = "700px")),
    column(6,plotlyOutput(NS(id,"map"), width = "1000px", height = "700px")),
  )) 
}            

# Define server function
exploreServer <- function(id){
  moduleServer(id, function(input, output, session){
  #word cloud
  output$wordcloud <-renderPlotly({
    type <- unlist(input$datatype)
    period <- unlist(input$timeperiod)
    #text transform: convert dataframe to corpus
    docs <- Corpus(VectorSource(as.character(raw_text%>%
                                filter(Datatype %in% type & time_bin %in% period)$clean_message)))
    #build a term-document matrix
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    # words and frequency dataframe
    df <- data.frame(word = names(words),freq=words)
    #generate word cloud
    set.seed(1234)
    wordcloud(words = df$word, freq = df$freq, min.freq = 10,max.words=300,
              random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
    ggplotly(wordcloud)
    
  })
  
