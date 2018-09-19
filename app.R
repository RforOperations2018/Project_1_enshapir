# ----------------------
#enshapir
# 
# project 1
#-----------------------

library(shiny)
library(shinydashboard)
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
Resturant.load$`INSPECTION DATE` <- as.Date(Resturant.load$`INSPECTION DATE`)


# Define dashboard UI
header <- dashboardHeader()

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Current Performance"), tabName = "Current Performance", icon = icon("dashboard"),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
            badgeColor = "green")
  ),
  
  menuItem('b',
           tabName = 'b',
           icon = icon('line-chart'),
           menuSubItem(selectInput("SelectedRace",
                                   "Race:",
                                   choices = c("test1, test2"),
                                   multiple = T,
                                   selectize = T,
                                   selected = c("test1"))),
           menuSubItem('m',
                       tabName = 'm',
                       icon = icon('line-chart')))
)

body <- dashboardBody(
  tabItem(tabName = "Current Performance", 
    fluidRow(
      valueBoxOutput("TimeSinceInspection"),
      valueBoxOutput("GradeLastestInspection"),
      valueBoxOutput("LastestViolationCount")
    ),
    box(
      title = "Number of Violations Over Time", solidHeader = TRUE,
      collapsible = TRUE,
      plotlyOutput("ViolationsOverTime", height = 250)
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
   
 
}

# Run the application 
shinyApp(ui = ui, server = server)

