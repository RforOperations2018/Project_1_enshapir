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
    selectInput(inputId = "selectResturant", label = "Resturant", 
                choices = list("Choice 1" = 1, "Choice 2" = 2,
                               "Choice 3" = 3), selected = 1),
    menuItem(text = "Current Performance", 
             tabName = "CP",
             icon = icon("dashboard")),
    menuItem(text = "Historical Preformance Comparison", 
             tabName = "HPC", 
             icon = icon("th")),
    menuItem("Table of Resturant Data",
             tabName = 'table',
             icon = icon('line-chart'))
  )
)

body <- dashboardBody(tabItems(
  
  tabItem(tabName = "CP", 
    fluidRow(
      valueBoxOutput("TimeSinceInspection"),
      valueBoxOutput("GradeLastestInspection"),
      valueBoxOutput("LastestViolationCount")
    ),
    fluidRow(
    box(
      title = "Number of Violations Over Time", solidHeader = TRUE,
      collapsible = FALSE,
      plotlyOutput("ViolationsOverTime")
    ),
    box(
      title = "Noted Violations", solidHeader = TRUE,
      collapsible = FALSE,
      plotlyOutput("ViolationsOverTime")
    )
    )
  ),
  
  
  tabItem(tabName = "HPC", 
          box(
            title = "Number of Violations Over Time within Zip", solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("ViolationsOverTime", height = 250)
          ),
          box(
            title = "Number of Violations by Comparison by Cuisine", solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("ViolationsOverTime", height = 250)
          )
  ),
  
  tabItem(tabName = "table",
          fluidPage(
            box(title = "Selected Resturants Data", 
                dataTableOutput("table"), width = 12))
  )

  )
)


ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
   
 
}

# Run the application 
shinyApp(ui = ui, server = server)

