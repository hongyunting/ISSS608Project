library(sf)
library(tmap)
library(readr)
library(tidyverse)
library(magrittr)
library(rgdal)
library(raster)

bgmap <- raster("R/data/Geospatial/MC2-tourist_modified.tif")

mpAbila <- st_read("R/data/Geospatial", layer = "Abila") %>%
  st_make_valid()

emp <- read_csv("data/car-assignments.csv")
gps <- read_csv("data/gpsModified.csv")
gpsOri <-read_csv("data/gpsOriginal.csv")
gps$id <- as_factor(gps$id)
gpsOri$id <- as_factor(gpsOri$id)

AbilaUI <- function(id){
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(NS(id, "Date"), "Date", min=as.Date("2014-01-06"), max=as.Date("2014-01-19"),
                  value = as.Date("2014-01-06")),
      checkboxInput(NS(id, "show_route"),
                    label = "Show Abila Route",
                    value = TRUE),
      tags$div(class = "multicol",
               checkboxGroupInput(NS(id, "emp_data"),
                         label = "Choose GAStech Employee(s):",
                         choiceNames = paste(emp$FirstName," ", emp$LastName, "(",emp$CarID,")"),
                         choiceValues = emp$CarID,
                         selected = 1)),
      actionButton(NS(id,"selectall"), label="Select/Deselect all")
    ),
    mainPanel(
      tmapOutput(NS(id, "abilaPlot")),
    )
  )
}

AbilaServer <- function(id){
  moduleServer(id, function(input, output, session){
    
    output$abilaPlot <- renderTmap({
      
      # Plotting places of interest
      by_date_id <- gps %>%
        filter(date == input$Date & id == input$emp_data)
      by_date_id$date <- factor(as.Date(by_date_id$date))
      gps_sf_path <- st_as_sf(by_date_id, coords = c("long", "lat"), crs = 4326)  %>%
        st_cast("POINT")
      
      # Plotting selected employee's route
      by_date_id_gpsOri <- gpsOri %>%
        filter(date == input$Date & id == input$emp_data) 
      by_date_id_gpsOri$date <- factor(as.Date(by_date_id_gpsOri$date))
      gps_original_emp_path <-st_as_sf(by_date_id_gpsOri, coords = c("long", "lat"), crs = 4326) %>%
        group_by(id) %>%
        summarize(m = mean(Timestamp),
                  do_union=FALSE) %>%
        st_cast("LINESTRING")
      
      print(by_date_id_gpsOri)
      
      tm_shape(bgmap) + 
       tm_rgb(bgmap, r = 1, g = 2, b = 3, # setting red to band 1, green to band 2, blue to band 3
               alpha = NA,
               saturation = 1,
               interpolate = TRUE, 
               max.value = 255) +
      if(input$show_route){
        tm_shape(mpAbila) +
          tm_lines() +
        tmap_options(max.categories = 45) +
          tm_shape(gps_original_emp_path)+
            tm_lines(col= "id") +
          tm_shape(gps_sf_path) +
            tm_dots(col ="id",
                  popup.vars=c("Date:"="date", "Time:"="time", "Day of Week:"="weekday", "Stopover duration (mins):"="diff"))
      }
      else{
        tmap_options(max.categories = 45) +
          tm_shape(gps_original_emp_path)+
            tm_lines(col= "id") +
          tm_shape(gps_sf_path) +
            tm_dots(col ="id",
                  popup.vars=c("Date:"="date", "Time:"="time", "Day of Week:"="weekday", "Stopover duration (mins):"="diff"))
      }
    })
  })
}