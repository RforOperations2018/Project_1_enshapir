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
Resturant.file.path <- "DOHMH_New_York_City_Restaurant_Inspection_Results_morn_side.xlsx"

Resturant.load <- read_xlsx(path = Resturant.file.path, sheet = 1, col_names = TRUE)
Resturant.load$`INSPECTION DATE` <- as.Date(Resturant.load$`INSPECTION DATE`)
Resturant.load <- Resturant.load %>% filter(`INSPECTION DATE` > "2014-01-01" & BORO == 'MANHATTAN')


# Define dashboard UI
header <- dashboardHeader()

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    selectizeInput(inputId = "selectResturant", 
                   label = "Resturant:", 
                   choices = sort(unique(Resturant.load$DBA)), 
                   selected = sort(unique(Resturant.load$DBA)[1]), 
                   multiple = FALSE,
                   options = list(maxItems = 1)),
    
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
  tabItem("CP",
          fluidRow(
            valueBoxOutput("TimeSinceInspection"),
            valueBoxOutput("GradeLast"),
            valueBoxOutput("ViolationCnt")
          ),
          fluidRow(
            tabBox(title = "",
                   width = 12,
                   tabPanel("Noted Major Violations", DT::dataTableOutput("vioTable")),
                   tabPanel("Number of Violations Over Time", plotlyOutput("ViolationsOverTime")))
          )
  ),
  
    tabItem(tabName = "HPC",
            fluidRow(
              tabBox(title = "",
                     width = 12,
                     tabPanel("Number of Violations Over Time within Zip", textOutput("VioZip"),
                              dateRangeInput(inputId = "date", 
                                        label ="Choose a Date",
                                        start = min(Resturant.load$`INSPECTION DATE`, na.rm = TRUE), 
                                        end = max(Resturant.load$`INSPECTION DATE`, na.rm = TRUE),
                                        min = min(Resturant.load$`INSPECTION DATE`, na.rm = TRUE),
                                        max = max(Resturant.load$`INSPECTION DATE`, na.rm = TRUE))),
                     tabPanel("Comparison of Violations by Cuisine", plotlyOutput("VioCuisine"),
                              selectInput(inputId = "selectCuis", 
                                          label = "Cuisine",
                                          choices = sort(unique(Resturant.load$`CUISINE DESCRIPTION`)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = unique(Resturant.load$`CUISINE DESCRIPTION`)[2]))
                        
                     )
                     
            )

    ),
  
  tabItem("table",
          fluidPage(
            box(title = "Selected Resturant Data", DT::dataTableOutput("table"), width = 12))
  )
)
)


 ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output, session=session) {

  resInput <- reactive({
    Resturant <- Resturant.load %>%
      # selectResturant filter
      filter(DBA == input$selectResturant)
  })
  
  cuInput <- reactive({
    Resturant <- Resturant.load %>%
      # selectResturant filter
      filter(`CUISINE DESCRIPTION` == input$selectCuis | `CUISINE DESCRIPTION` ==resInput()$`CUISINE DESCRIPTION`)
  })
  
  output$TimeSinceInspection <- renderValueBox({
    res <- resInput()
    resCurrent <- res %>%
      filter(`INSPECTION DATE` == max(res$`INSPECTION DATE`))
    currentDate <- Sys.Date()
    lastInspect <- resCurrent$`INSPECTION DATE`
    days.since.last <- as.numeric(currentDate - lastInspect)
    valueBox(subtitle = "Days Since Last Inspection", value = days.since.last, icon = icon("calendar"), color = "green")
  })

  output$GradeLast <- renderValueBox({
    res <- resInput()
    resCurrent <- res %>%
      filter(`INSPECTION DATE` == max(res$`INSPECTION DATE`))
    valueBox(subtitle = "Grade on Last Inspection", value = resCurrent$GRADE, icon = icon("id-card-o"), color = "green")
  })
  
  output$ViolationCnt <- renderValueBox({
    res <- resInput()
    resCurrent <- res %>%
      filter(`INSPECTION DATE` == max(res$`INSPECTION DATE`))
    valueBox(subtitle = "Violation Score (Lower is Better)", value = resCurrent$SCORE, icon = icon("exclamation-triangle "), color = "green")
  })
  # 
  # test1 %>% group_by(`INSPECTION DATE`) %>% 
  #   +     summarize(type = paste(sort(unique(`VIOLATION DESCRIPTION`)),collapse=", "))
  
  # Data table of Resturnat Inspections
  output$vioTable <- DT::renderDataTable({
    res <- resInput()
    resCurrent <- res %>%
      subset(`INSPECTION DATE` == max(res$`INSPECTION DATE`))
    subset(resCurrent, select = c(`VIOLATION CODE`, `VIOLATION DESCRIPTION`))
  })
  
  # A plot showing the violations overtime of the resturant
  output$ViolationsOverTime <- renderPlotly({
    data1 <- resInput()
    ggplotly(
      ggplot(data = data1, mapping = aes(x=`INSPECTION DATE`, y=SCORE))+
      geom_line()+
      labs(x="Inspection Dates", y="Violation Score"))
  })
  
  # A plot showing the violations overtime of the resturant
  output$VioCuisine <- renderPlotly({
    data1 <- cuInput()
    ggplotly(
      ggplot(data = data1, mapping = aes(x=`INSPECTION DATE`, y= SCORE, color=`CUISINE DESCRIPTION`))+
        geom_line() +
        labs(x="Inspection Dates", y="Violation Score"))
  })
  # observeEvent(input$selectResturant, {
  #   res1 <- resInput()
  #   radioChioce <- subset(Resturant.load, -c(res1$`CUISINE DESCRIPTION`)
  #   updateRadioButtons(session, inputId = "selectCuis", 
  #                      label = "Cuisine:", 
  #                      choices = radioChioce, 
  #                      selected = radioChioce[1])
  # })

           
  observeEvent(input$reset, {
    updateSelectInput(session, "SelectedRace", selected = c("ASIAN", "BLACK"))
    showNotification("You have successfully reset to show all races", type = "message")
  })

  
  
  
  # Data table of Resturnat Inspections
  output$table <- DT::renderDataTable({
    subset(resInput(), select = c(DBA,`CUISINE DESCRIPTION`, `VIOLATION CODE`, SCORE, GRADE, `INSPECTION DATE`,`INSPECTION TYPE`))
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)

