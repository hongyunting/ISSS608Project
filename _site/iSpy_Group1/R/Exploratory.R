library(shiny)
library(tidyverse)
library(plotly)
library(dplyr)
library(tools)
library(lubridate)
library(stringi)
library(clock)
library(shinyWidgets)

cc <- read_csv("R/data/cc_data.csv")
locationmapping <- read_csv("R/data/LocationMapping.csv")
cc$timestamp <- date_time_parse(cc$timestamp,
                                zone = "",
                                format = "%m/%d/%Y %H:%M")
df1 <- left_join(cc,locationmapping,
                 by = c('location' = "Location"))
df1$date <- as.Date(df1$timestamp, format="%m/%d/%Y")
df1$location <- stri_trans_general(df1$location,  "latin-ascii")

#adding features of the data
df1$hour <- strftime(df1$timestamp, format = "%H")
df1$datehour <- strftime(df1$timestamp, format = "%d-%H")
df1$period <- cut(as.numeric(df1$hour),
                  breaks = c(0,5,11,14,19,20,23),
                  labels = c("midnight",
                             "morning",
                             "lunch",
                             "afternoon",
                             "dinner",
                             "night"))

df1$last4ccnum <- as_factor(df1$last4ccnum)


exploreUI <- function(id){
  fluidPage(
  fluidRow(
    
    column(4, 
           pickerInput(
             NS(id,"category"),
             label = div(style = "font-size:10px;",
                         "Category"),
             choices = c("F&B" = "F&B",
                         "Life" = "Life",
                         "Work" = "Work"),
             selected = c("Work", "F&B", "Work"),
             options = list(`actions-box` = TRUE),
             multiple = T
           )),
    column(4,
           pickerInput(
             NS(id, "timeperiod"),
             label = "Timeperiod",
             choices = c("morning" = "morning",
                         "lunch" = "lunch",
                         "afternoon" = "afternoon",
                         "dinner" = "dinner",
                         "night" = "night",
                         "midnight" = "midnight"),
             selected = c("lunch", "morning", "midnight","afternoon","dinner","night"),
             options = list(`actions-box` = TRUE),
             multiple = T
           ))),
  fluidRow(
    column(6,
           plotlyOutput(NS(id,"jitterplot"), width = "1000px", height = "700px")),
    column(6,
           plotlyOutput(NS(id,"barplot"),width = "900px", height = "700px"))
    
  )) 
}  



exploreServer <- function(id){
  moduleServer(id, function(input, output, session){
    output$barplot <- renderPlotly({
      df3 <- df1 %>%
        group_by(location, period, Category) %>%
        summarise(Count = n())
      
      #unlist
      cats <- unlist(input$category)
      periods <- unlist(input$timeperiod)
      barplots <- ggplot(df3 %>%
                           filter(Category %in% cats & period %in% periods),
                         aes(x = location,
                             y = Count)) +
        geom_bar(stat = "identity",
                 color = "black") +
        geom_col(aes(fill = period)) + 
        scale_fill_brewer(palette = "Spectral") +
        theme_light()+
        theme(axis.text.x = element_text(angle = 90, 
                                         vjust = 0.5, 
                                         hjust = 1))
      
      ggplotly(barplots)
      
    })
    output$jitterplot <- renderPlotly({
      cats <- unlist(input$category)
      periods <- unlist(input$timeperiod)
      p <- ggplot(df1 %>%
                    filter(Category %in% cats & period %in% periods),
                  aes(x = location,
                      y = price,
                      #group = Category,
                      color = location,
                      text = paste("Price: ", price, 
                                   "<br>", 
                                   "CC: ", last4ccnum, 
                                   "<br>", 
                                   "Timestamp: ", timestamp
                      )
                  )
      ) + 
        geom_jitter(size = 0.5) +
        theme(axis.text.x = element_text(angle = 90, 
                                         vjust = 0.5, 
                                         hjust = 1))
      ggplotly(p,tooltip = c('text'))
      
    })
    
  })
}
  