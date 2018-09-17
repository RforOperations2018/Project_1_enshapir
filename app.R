# ----------------------
#enshapir
# 
# project 1
#-----------------------

library(shiny)
library(tidyverse)
library(readxl)
library(rjson)
library(plotly)
library(DT)

#DOHMH New York City Restaurant Inspection Results
#https://data.cityofnewyork.us/Health/DOHMH-New-York-City-Restaurant-Inspection-Results/43nn-pn8j

#file path for data
Resturant.file.path <- file.path('data', 'DOHMH_New_York_City_Restaurant_Inspection_Results.xlsx', fsep = .Platform$file.sep)

Resturant.load <- read_xlsx(path = Resturant.file.path, sheet = 1, col_names = TRUE)

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

