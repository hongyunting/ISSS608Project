#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(rsconnect)

ui <- fluidPage(
    
    #Navbar structure for UI
    navbarPage("iSpy - Alibab, Kronos", theme = shinytheme("lumen"),
               tabPanel("Exploratory Data Analysis", fluid = TRUE), 
               tabPanel("GPS Tracking", fluid = TRUE, 
                        titlePanel("Tracing GAStech's Assigned Car"),
                        tags$head(
                          tags$style(HTML("
                             .multicol {
                               -webkit-column-count: 3; /* Chrome, Safari, Opera */
                               -moz-column-count: 3; /* Firefox */
                               column-count: 3;
                            }
                       "))),
                        AbilaUI("abilaPlot")),
               tabPanel("Text Analysis", fluid = TRUE))
              
)

server <- function(input, output, session) {
  AbilaServer("abilaPlot")
}

# Run the application 
shinyApp(ui = ui, server = server)
