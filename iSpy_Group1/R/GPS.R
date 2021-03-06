library(dplyr)
library(sf)
library(tmap)
library(readr)
library(tidyverse)
library(magrittr)
library(rgdal)
library(raster)
library(timevis)
library(shinybusy)
library(shinyWidgets)

#Import data
bgmap <- raster("R/data/Geospatial/MC2-tourist_modified.tif")

mpAbila <- st_read("R/data/Geospatial", layer = "Abila") %>%
  st_make_valid()

emp <- read_csv("R/data/car-assignments.csv")

empLocation <- read_csv("R/data/empLocation.csv")
empLocation$id <- as_factor(empLocation$id)
empLocation$date <- as.Date(empLocation$date, format = "%d/%m/%y")

gps <-read_csv("R/data/gps.csv")
gps$id <- as_factor(gps$id)
gps$date <- as.Date(gps$date, format = "%d/%m/%y")

ccLoyalty <-read_csv("R/data/ccLoyalty.csv")
ccLoyalty$date <- as.Date(ccLoyalty$date, format = "%d/%m/%y")

ccLoyalty <- ccLoyalty %>%
  dplyr::select(CarID, date, time, period, location, price, last4ccnum, loyaltynum.x)

periodStr <- c("morning", "lunch", "afternoon", "dinner", "night", "midnight")

# UI
AbilaUI <- function(id){
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(NS(id, "Date"), "Date Range", min=as.Date("2014-01-06"), max=as.Date("2014-01-19"),
                  value = c(as.Date("2014-01-06"), as.Date("2014-01-06"))),
      sliderTextInput(NS(id, "timeperiod"), 
                      label = "Timeperiod Range (Morning, Lunch, Afternoon, Dinner, Night, Midnight)",
                      choices = c("morning" = "morning",
                                  "lunch" = "lunch",
                                  "afternoon" = "afternoon",
                                  "dinner" = "dinner",
                                  "night" = "night",
                                  "midnight" = "midnight"),
                      selected = c("morning", "midnight")),
      checkboxInput(NS(id, "show_route"),
                    label = "Show Abila Route",
                    value = TRUE),
      actionLink(NS(id,"selectall"), label="Select/Deselect all"),
      tags$div(class = "multicol",
               checkboxGroupInput(NS(id, "emp_data"),
                         label = "Choose GAStech Employee(s):",
                         choiceNames = paste(emp$FirstName," ", emp$LastName, "(",emp$CarID,")"),
                         choiceValues = emp$CarID,
                         selected = c(1))),
      actionButton(NS(id,"apply_changes"), label="Apply Changes"),
      add_busy_bar(color = "grey", height = "3px")
    ),
    mainPanel(
      tmapOutput(NS(id, "abilaPlot")),
      DT::dataTableOutput(NS(id, "ccTable")),
      timevisOutput(NS(id, "timeline"))
    )
  )
}

# Server
AbilaServer <- function(id){
  moduleServer(id, function(input, output, session){
  
    observe({
      if(input$selectall == 0) return(NULL) 
      else if (input$selectall%%2 == 0)
      {
        updateCheckboxGroupInput(session,"emp_data","Choose GAStech Employee(s):",choiceNames = paste(emp$FirstName," ", emp$LastName, "(",emp$CarID,")"),
                                 choiceValues = emp$CarID)
      }
      else
      {
        updateCheckboxGroupInput(session,"emp_data","Choose GAStech Employee(s):",choiceNames = paste(emp$FirstName," ", emp$LastName, "(",emp$CarID,")"),
                                 choiceValues = emp$CarID, selected = emp$CarID)
      }
    })
    
    output$abilaPlot <- renderTmap({
      
      input$apply_changes
      isolate({
      
        periodIndex <- match(input$timeperiod, periodStr)
        
        if(length(periodIndex) > 1)
          periodStr2 <- periodStr[periodIndex[1]:periodIndex[2]]
        else
          periodStr2 <-periodStr[periodIndex]
        
        # Plotting places of interest
        by_date_id <- empLocation %>%
          filter(between(date, input$Date[1],input$Date[2]) & id %in% input$emp_data & period %in% periodStr2)
        by_date_id$date <- factor(as.Date(by_date_id$date))
        gps_sf_path <- st_as_sf(by_date_id, coords = c("long", "lat"), crs = 4326)  %>%
          st_cast("POINT")
        
        # Plotting selected employee's route
        by_date_id_gps <- gps %>%
          filter(between(date, input$Date[1],input$Date[2]) & id %in% input$emp_data & period %in% periodStr2) 
        by_date_id_gps$date <- factor(as.Date(by_date_id_gps$date))
        
        gps_emp_path <-st_as_sf(by_date_id_gps, coords = c("long", "lat"), crs = 4326) %>%
          group_by(id) %>%
          summarize(m = mean(Timestamp),
                    do_union=FALSE) %>%
          st_cast("LINESTRING")

        tm_shape(bgmap) + 
          tm_rgb(bgmap, r = 1, g = 2, b = 3, # setting red to band 1, green to band 2, blue to band 3
                 alpha = NA,
                 saturation = 1,
                 interpolate = TRUE, 
                 max.value = 255) +
          if(input$show_route){
            tm_shape(mpAbila) +
              tm_lines(col = "lightgrey") +
              tmap_options(max.categories = 45) +
              tm_shape(gps_emp_path)+
              tm_lines(col= "id") +
              tm_shape(gps_sf_path) +
              tm_dots(col ="id",
                      popup.vars=c("Date:"="date", "Time:"="time", "Day of Week:"="weekday", "Stopover duration (mins):"="diff"))
          }
        else{
          tmap_options(max.categories = 45) +
            tm_shape(gps_emp_path)+
            tm_lines(col= "id") +
            tm_shape(gps_sf_path) +
            tm_dots(col ="id",
                    popup.vars=c("Date:"="date", "Time:"="time", "Day of Week:"="weekday", "Stopover duration (mins):"="diff"))
        }
      })
    })
    
    output$ccTable <- DT::renderDataTable({
      input$apply_changes
      isolate({
        
        periodIndex <- match(input$timeperiod, periodStr)
        
        if(length(periodIndex) > 1)
          periodStr2 <- periodStr[periodIndex[1]:periodIndex[2]]
        else
          periodStr2 <-periodStr[periodIndex]

        filteredData <- ccLoyalty %>%
          filter(between(date, input$Date[1],input$Date[2]) & CarID %in% input$emp_data & period %in% periodStr2)
        
        DT::datatable(data = filteredData,
                      options = list(pageLength = 10),
                      rownames = FALSE)  
      })
    })
  })
}