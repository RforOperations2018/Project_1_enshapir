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
    
    menuItem(text = "Current Performance", 
             tabName = "plot",
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
            valueBoxOutput("height")
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

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  resInput <- reactive({
    Resturant <- Resturant.load %>%
      # selectResturant filter
      filter(DBA == input$selectResturant)
  })
  # Reactive melted data
  mwInput <- reactive({
    swInput() %>%
      melt(id = "name")
  })
  # A plot showing the mass of characters
  output$plot_mass <- renderPlotly({
    dat <- subset(mwInput(), variable == "mass")
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  # A plot showing the height of characters
  output$plot_height <- renderPlotly({
    dat <- subset(mwInput(),  variable == "height")
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  # Data table of characters
  output$table <- DT::renderDataTable({
    subset(swInput(), select = c(name, height, mass, birth_year, homeworld, species, films))
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
