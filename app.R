#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(DT)

# Define dashboard UI

  
header <- dashboardHeader()

sidebar <- dashboardSidebar()

body <- dashboardBody()

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
   
 
}

# Run the application 
shinyApp(ui = ui, server = server)

