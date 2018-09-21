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
library(shinythemes)

#DOHMH New York City Restaurant Inspection Results
#https://data.cityofnewyork.us/Health/DOHMH-New-York-City-Restaurant-Inspection-Results/43nn-pn8j

#file path for data
Resturant.file.path <- file.path('data', 'DOHMH_New_York_City_Restaurant_Inspection_Results.xlsx', fsep = .Platform$file.sep)

Resturant.load <- read_xlsx(path = Resturant.file.path, sheet = 1, col_names = TRUE)
Resturant.load$`INSPECTION DATE` <- as.Date(Resturant.load$`INSPECTION DATE`)
Resturant.load <- Resturant.load %>% filter(`INSPECTION DATE` > "2014-01-01" & BORO == 'MANHATTAN')


# Define dashboard UI
header <- dashboardHeader()

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    selectizeInput(inputId = "selectResturant", 
                   label = "Resturant:", 
                   choices = sort(unique(Resturant.load$DBA))[1:2000], 
                   selected = sort(unique(Resturant.load$DBA)[1]), 
                   multiple = FALSE,
                   options = list(maxItems = 1)),
     # selectInput(inputId = "selectResturant", 
     #             label = "Resturant", 
     #             choices = c(sort(unique(Resturant.load$DBA))),
     #             selected = sort(unique(Resturant.load$DBA)[1])),
     
    menuItem(text = "Current Performance1", 
             tabName = "plot",
             icon = icon("dashboard")),
    
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
  tabItem("plot",
          fluidRow(
            infoBoxOutput("mass"),
            valueBoxOutput("TimeSinceInspection")
          ),
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Mass", plotlyOutput("plot_mass")),
                   tabPanel("Height", plotlyOutput("plot_height")))
          )
  ),
  tabItem("table",
          fluidPage(
            box(title = "Selected Character Stats", DT::dataTableOutput("table"), width = 12))
  )
)
)


# 
# 
# 
# 
# 
# 
# body <- dashboardBody(tabItems(
#   tabItem("plot",
#           fluidRow(
#             infoBoxOutput("mass"),
#             valueBoxOutput("height")
#           ),
#           fluidRow(
#             tabBox(title = "Plot",
#                    width = 12,
#                    tabPanel("Mass", plotlyOutput("plot_mass")),
#                    tabPanel("Height", plotlyOutput("plot_height")))
#           )
#   ),
#   
#   tabItem(tabName = "CP", 
#     fluidRow(
#       box(valueBoxOutput(outputId = "TimeSinceInspection", width = 4)),
#       box(valueBoxOutput(outputId = "GradeLastestInspection", width = 4)),
#       box(valueBoxOutput(outputId = "LastestViolationCount", width = 4))
#     ),
#     fluidRow(
#     box(
#       title = "Number of Violations Over Time", solidHeader = TRUE,
#       collapsible = FALSE,
#       plotlyOutput("ViolationsOverTime")
#     ),
#     box(
#       title = "Noted Violations", solidHeader = TRUE,
#       collapsible = FALSE,
#       plotlyOutput("ViolationsOverTime")
#     )
#     )
#   ),
#   
#   
#   tabItem(tabName = "HPC", 
#           box(
#             title = "Number of Violations Over Time within Zip", solidHeader = TRUE,
#             collapsible = FALSE,
#             plotlyOutput("ViolationsOverTime", height = 250)
#           ),
#           box(
#             title = "Number of Violations by Comparison by Cuisine", solidHeader = TRUE,
#             collapsible = FALSE,
#             plotlyOutput("ViolationsOverTime", height = 250)
#           )
#   ),
#   
#   tabItem(tabName = "table",
#           fluidPage(
#             box(title = "Selected Resturants Data", 
#                 dataTableOutput("table"), width = 12))
#   )
# 
#   )
# )


 ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output, session=session) {

  resInput <- reactive({
    Resturant <- Resturant.load %>%
      # selectResturant filter
      filter(DBA == input$selectResturant)
  })

  output$TimeSinceInspection <- renderValueBox({
    res <- resInput()
    currentDate <- Sys.Date()
    lastInspect <- res$`INSPECTION DATE`
    days.since.last <- as.numeric(currentDate - lastInspect)
    valueBox(subtitle = "Days Since Last Inspection", value = days.since.last, icon = icon("sort-numeric-asc"), color = "green")
  })

  output$GradeLastestInspection <- renderValueBox({
    days.since.last <- as.numeric(currentDate - lastInspect)
    valueBox(subtitle = "Days Since Last Inspection", value = 45, icon = NULL, color = "green")
  })

  # output$LastestViolationCount <- renderValueBox({
  #   res <- resInput()
  #   currentDate <- Sys.Date()
  #   lastInspect <- res$`INSPECTION DATE`
  #   days.since.last <- as.numeric(currentDate - lastInspect)
  #   valueBox(subtitle = "Days Since Last Inspection", value = 57, icon = NULL, color = "green")
  # })

  output$LastestViolationCount <- renderValueBox({
    sw <- swInput()
    num <- round(mean(sw$height, na.rm = T), 2)

    valueBox(subtitle = "Avg Height", value = num, icon = icon("sort-numeric-asc"), color = "green")
  })

# Mass mean info box
output$mass <- renderInfoBox({
  sw <- swInput()
  num <- round(mean(sw$mass, na.rm = T), 2)

  infoBox("Avg Mass", value = num, subtitle = paste(nrow(sw), "characters"), icon = icon("balance-scale"), color = "purple")
})
# Height mean value box
output$height <- renderValueBox({
  sw <- swInput()
  num <- round(mean(sw$height, na.rm = T), 2)

  valueBox(subtitle = "Avg Height", value = num, icon = icon("sort-numeric-asc"), color = "green")
})


}



# Run the application 
shinyApp(ui = ui, server = server)

