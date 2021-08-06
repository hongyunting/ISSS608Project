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

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    #Navbar structure for UI
    navbarPage("iSpy Analysis", theme = shinytheme("lumen"),
               tabPanel("Exploratory", fluid = TRUE), 
               tabPanel("GPS Tracking", fluid = TRUE, 
                        sidebarLayout(
                            sidebarPanel(
                                titlePanel("Filter By")
                                        ),
                            mainPanel()
                        )),
               tabPanel("Text Analysis", fluid = TRUE))
              
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

}

# Run the application 
shinyApp(ui = ui, server = server)
