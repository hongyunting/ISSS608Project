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
                         label = "GAStech Employees",
                         choices = c(emp$CarID),
                         selected = 1))
    ),
    mainPanel(
      tmapOutput(NS(id, "abilaPlot")),
    )
  )
}

AbilaServer <- function(id){
  moduleServer(id, function(input, output, session){
    output$abilaPlot <- renderTmap({
      
      by_date <- gps %>%
        filter(date == input$Date & id == input$emp_data)
      
      by_date$date <- factor(as.Date(by_date$date))
      
      gps_sf_path <- st_as_sf(by_date, coords = c("long", "lat"), crs = 4326)  %>%
        st_cast("POINT")
      
      tm_shape(bgmap) + 
       tm_rgb(bgmap, r = 1, g = 2, b = 3, # setting red to band 1, green to band 2, blue to band 3
               alpha = NA,
               saturation = 1,
               interpolate = TRUE, 
               max.value = 255) +
        tm_layout(frame.lwd = 50, asp=0) +
      if(input$show_route){
        tm_shape(mpAbila) +
        tm_lines() +
        tmap_options(max.categories = 49) +
          tm_shape(gps_sf_path) +
          tm_dots(col ="id",
                  popup.vars=c("Date:"="date", "Time:"="time", "Day of Week:"="weekday", "Stopover duration (mins):"="diff"))
      }
      else{
        tmap_options(max.categories = 49) +
          tm_shape(gps_sf_path) +
          tm_dots(col ="id",
                  popup.vars=c("Date:"="date", "Time:"="time", "Day of Week:"="weekday", "Stopover duration (mins):"="diff"))
      }
    })
  })
}