### data clean
#### import packages
packages= c()

packages = c('DT','tidytext','widyr','dplyr','wordcloud',
             'ggwordcloud','textplot','lubridate','hms','tidyverse','tidygraph',
             'ggraph','igraph','LDAvis','servr','dplyr','stringi',
             'raster','sf','clock','tmap','data.table','textclean','tm',
             'wordcloud','wordcloud2','text2vec','topicmodels','tidytext',
             'textmineR','quanteda','BTM','textplot','concaveman','ggwordcloud',
             'qdapDictionaries','textstem','devtools','ggiraph',
             'plotly','igraph', 'tidygraph','visNetwork','udpipe','grid',
             'SnowballC','proustr')

for (p in packages){
    if (!require(p,character.only = T)){
        install.packages(p)    
    }
    library(p,character.only = T)
}

#### import data
segment1 <- read_csv('R/data/csv-1700-1830.csv')
segment2 <- read_csv('R/data/csv-1831-2000.csv')
segment3 <-read_csv('R/data/csv-2001-2131.csv')

#### combine all these 3 segments data together 
raw_text <- rbind(segment1,segment2,segment3)

#### Wrangling time
raw_text$`date(yyyyMMddHHmmss)`<-ymd_hms(raw_text$`date(yyyyMMddHHmmss)`)


#### 1.1 clean data,raw_text
raw_text$clean_message <-raw_text$message%>%
    tolower()%>%#change all messages to lowercase
    replace_contraction()%>%#remove short form
    replace_word_elongation()%>% #remove the same letter appears unnecessarily, eg.'loooook' to 'look'
    str_squish()%>% #re3moves space from start and end of string
    lemmatize_strings()%>%#perform lemmatization
    removeWords(stopwords('english'))#%>%#remove stopwords

#### 1.2 clean data,remove keywords in the message - these messages are identified as junk messages
raw_text$clean_message <-raw_text$message %>% 
    #remove rt @ in the message, replace with""
    str_replace_all("RT @([A-Za-z]+[A-Za-z0-9_-]+)(?![A-Za-z0-9_]*\\.)","")%>%
    str_replace_all("rt @([A-Za-z]+[A-Za-z0-9_-]+)(?![A-Za-z0-9_]*\\.)","")%>%
    #remove @ in the message, replace with""
    str_replace_all("@([A-Za-z]+[A-Za-z0-9_-]+)(?![A-Za-z0-9_]*\\.)","")%>%
    #remove # in the message, replace with""
    str_replace_all("#([A-Za-z]+[A-Za-z0-9_]+)(?![A-Za-z0-9_]*\\.)","")%>%
    #remove stop word:the/The/to/ of / is/ in /you /and/ have/ at /are /for/ on/your/ it/ that /be with /more 
    gsub(pattern ='The',replacement = "",raw_text$message)%>%
    gsub(pattern ='the',replacement = "",raw_text$message)%>%
    gsub(pattern ='to',replacement = "",raw_text$message)%>%
    gsub(pattern ='of',replacement = "",raw_text$message)%>%
    gsub(pattern ='is',replacement = "",raw_text$message)%>%
    gsub(pattern ='in',replacement = "",raw_text$message)%>%
    gsub(pattern ='you',replacement = "",raw_text$message)%>%
    gsub(pattern ='your',replacement = "",raw_text$message)%>%
    gsub(pattern ='and',replacement = "",raw_text$message)%>%
    gsub(pattern ='have',replacement = "",raw_text$message)%>%
    gsub(pattern ='at',replacement = "",raw_text$message)%>%
    gsub(pattern ='are',replacement = "",raw_text$message)%>%
    gsub(pattern ='for',replacement = "",raw_text$message)%>%
    gsub(pattern ='on',replacement = "",raw_text$message)%>%
    gsub(pattern ='it',replacement = "",raw_text$message)%>%
    gsub(pattern ='that',replacement = "",raw_text$message)%>%
    gsub(pattern ='be',replacement = "",raw_text$message)%>%
    gsub(pattern ='with',replacement = "",raw_text$message)%>%
    gsub(pattern ='more',replacement = "",raw_text$message)

#### 1.3 stemming message
pr_stem_words(raw_text,clean_message,language = "english")

### 1.4 create time filter as per hour
raw_text$time_bin = cut(raw_text$`date(yyyyMMddHHmmss)`, breaks="60 mins")
raw_text$time_bin<-raw_text$time_bin %>% str_replace_all("2014-01-23 ","")
raw_text$time_bin <- raw_text$time_bin %>%
    gsub(pattern ='17:00:00',replacement = "5pm",raw_text$message)%>%
    gsub(pattern ='18:00:00',replacement = "6pm",raw_text$message)%>%
    gsub(pattern ='19:00:00',replacement = "7pm",raw_text$message)%>%
    gsub(pattern ='20:00:00',replacement = "8pm",raw_text$message)%>%
    gsub(pattern ='21:00:00',replacement = "9pm",raw_text$message)

#### 1.5 split into blog and text transcripts of emergency call
blog <- filter(raw_text,type=='mbdata')
call <- filter(raw_text,type=='ccdata')

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

# Define UI for application
eventUI <- function(id){
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(NS(id,'datatype'),label='Datatype',
                  choices =c('mbdata' = 'mbdata',
                             'ccdata' = 'ccdata'),
                  selected = "mbdata",
                  options = list(`actions-box` = TRUE),
                  multiple = T),
      pickerInput(NS(id,'timeperiod'),label='Timeperiod',
                  choices =c('5pm' = '5pm',
                             '6pm' = '6pm',
                             '7pm' = '7pm',
                             '8pm' = '8pm',
                             '9pm' = '9pm'),
                  selected = "5pm",
                  options = list(`actions-box` = TRUE),
                  multiple = T),
      sliderInput(NS(id,"freq"),
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput(NS(id, "max"),
                  "Maximum Number of Words:",
                  min = 1,  max = 200,  value = 100)
    ),
    mainPanel(
      plotOutput(NS(id,"wordcloud"), width = "1000px", height = "700px")
    )
  )
}            

# Define server function
eventServer <- function(id){
  moduleServer(id, function(input, output, session){
    #word cloud
    output$wordcloud <-renderPlot({
      types <- unlist(input$datatype)
      period <- unlist(input$timeperiod)
      
      #text transform: convert dataframe to corpus
      cleanMsg <- raw_text %>% 
        filter(type %in% types & time_bin %in% period)
      
      docs <- Corpus(VectorSource(as.character(cleanMsg$clean_message)))
      
      #build a term-document matrix
      dtm <- TermDocumentMatrix(docs) 
      
      matrix <- as.matrix(dtm) 
      words <- sort(rowSums(matrix),decreasing=TRUE) 
      # words and frequency dataframe
      df <- data.frame(word = names(words),freq=words)
      #generate word cloud
      set.seed(1234)
      
      wordcloud(words = df$word, freq = df$freq, min.freq = input$freq, max.words=input$max,
                colors=brewer.pal(8, "Dark2"))
      
    })
  })
}

