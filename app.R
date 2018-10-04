# ----------------------
#enshapir
# 
# project 1
#-----------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(jsonlite)
library(plotly)
library(DT)
library(shinythemes)
library(httr)
library(htmltools)

# only boro of brooklyn and limits 10
# https://data.cityofnewyork.us/resource/9w7m-hzhe.json?boro=BROOKLYN&$limit=10

#41615257 40813994 40685734 41698319 50048821 50003527 41561808 40824179 40388091 50066109 41241757 40918579 41365100
#  $where=annual_salary between '40000' and '60000'

#  $where=camis in('41615257','40813994','40685734','41698319','50048821','50003527','41561808','40824179','40388091','50066109','41241757','40918579','41365100')

$select=location, magnitude AS richter

url <- paste0("https://data.cityofnewyork.us/resource/9w7m-hzhe.json",'?',"$select=camis, dba","$where=camis in('41615257','40813994','40685734','41698319','50048821','50003527','41561808','40824179','40388091','50066109','41241757','40918579','41365100')",'&$limit=10000')


r <- RETRY("GET", url = URLencode(url))
# Extract Content
c <- content(r, "text")
# Basic gsub to make NA's consistent with R
json <- gsub('NaN', 'NA', c, perl = TRUE)
# Create Dataframe
test <- data.frame(fromJSON(json))





#DOHMH New York City Restaurant Inspection Results
#https://data.cityofnewyork.us/Health/DOHMH-New-York-City-Restaurant-Inspection-Results/43nn-pn8j

#file path for data
Resturant.file.path <- "DOHMH_New_York_City_Restaurant_Inspection_Results_morn_side.xlsx"

#loading in resturant data
Resturant.load <- read_xlsx(path = Resturant.file.path, sheet = 1, col_names = TRUE)
Resturant.load$`INSPECTION DATE` <- as.Date(Resturant.load$`INSPECTION DATE`)
Resturant.load <- Resturant.load %>% filter(`INSPECTION DATE` > "2014-01-01" & BORO == 'MANHATTAN')
# Mutate could have done your filtering here as well.

# Define dashboard UI
# Iknow the header is mostly useless but this is where your title goes!
header <- dashboardHeader()

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    selectizeInput(inputId = "selectResturant", 
                   label = "Resturant:", 
                   choices = sort(unique(Resturant.load$DBA)), 
                   selected = sort(unique(Resturant.load$DBA)[1]), 
                   multiple = FALSE,
                   options = list(maxItems = 1)),
    
     menuItem(text = "Restaurant Performance", 
             tabName = "CP",
             icon = icon("dashboard")),
    
     menuItem(text = "Performance Comparison", 
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
            #value boxes for Time since last inspection, grade on most recent inspection, violation score on last inspection 
            valueBoxOutput("TimeSinceInspection"),
            valueBoxOutput("GradeLast"),
            valueBoxOutput("ViolationCnt")
          ),
          fluidRow(
            tabBox(title = "",
                   width = 12,
              # a table of the noted critical violations on the most recent inspection 
                   tabPanel("Noted Crititcal Violations", DT::dataTableOutput("vioTable")),
              #a historical look at the chosen restaurants violation score
                   tabPanel("Number of Violation Score Over Time", plotlyOutput("ViolationsOverTime"),
                            uiOutput("dateRange")
                            )
                   )
          )
  ),
  
    tabItem(tabName = "HPC",
            fluidRow(
              tabBox(title = "",
                     width = 12,
                     #presenting a graph where the user can compare resturants by Critical Violations 
                     tabPanel("Number of Crititcal Violations Over Time Comparison",plotlyOutput("Viocrit"),
                              uiOutput("streetComp1")
                              ),
                     #presenting a graph where the user can compare resturants violation score by Cuisine type
                     tabPanel("Comparison of Violations by Cuisine", plotlyOutput("VioCuisine"),
                              uiOutput("selectCuis1")
                              )
                     )
                     
            )

    ),
  #a table of all inspection information for the chosen restaurant
  tabItem("table",
          fluidPage(
            box(title = "Selected Resturant Data", DT::dataTableOutput("table"), width = 12))
  )
)
)


 ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output, session=session) {

  #creating a dataset of a single restaurant's inspections
  resInput <- reactive({
    Resturant <- Resturant.load %>%
      # selectResturant filter
      filter(DBA == input$selectResturant)
  })
  
  #creating a dataset of restaurants selected by cuisine type
  cuInput <- reactive({
    Resturant <- Resturant.load %>%
      # selectResturant filter
      filter(`CUISINE DESCRIPTION` == input$selectCuis | `CUISINE DESCRIPTION` ==resInput()$`CUISINE DESCRIPTION`)
  })
  
  #creating a dataset with counts of critical inputs per resturant
  resInput2 <- reactive({
    Resturant <- Resturant.load %>% 
      group_by(DBA, `INSPECTION DATE`, STREET, SCORE) %>% 
      summarise(crits = sum(`CRITICAL FLAG`=="Critical")) %>% 
      filter(DBA %in% input$streetComp | DBA %in% resInput()$DBA)
  })
  
  #A date range selection for the currently selected restaurant’s inspection dates
  output$dateRange <- renderUI({
    dateRangeInput(inputId = "dateRange1",
                   label = "Pick a Date Range",
                   start = min(resInput()$`INSPECTION DATE`, na.rm = TRUE), 
                   end = max(resInput()$`INSPECTION DATE`, na.rm = TRUE),
                   min = min(resInput()$`INSPECTION DATE`, na.rm = TRUE),
                   max = max(resInput()$`INSPECTION DATE`, na.rm = TRUE)
                   )
    })
  
  #A selection box for resturant that does not show the currently selected restaurant as an option
  output$streetComp1 <- renderUI({
    resChoice <-  Resturant.load %>% filter(DBA != resInput()$DBA)
    
    selectInput(inputId = "streetComp",
                   label = "Resturant Comparison",
                   choices = sort(unique(resChoice$DBA)),
                   multiple = TRUE,
                   selectize = TRUE,
                   selected = unique(resChoice$DBA)[1]
                   )
  })
  
  #A selection box for cuisine type that does not show the currently selected restaurant’s cuisine as an option
  output$selectCuis1 <- renderUI({
    cuChoice <-  Resturant.load %>% filter(`CUISINE DESCRIPTION` != resInput()$`CUISINE DESCRIPTION`)
    
    selectInput(inputId = "selectCuis",
                label = "Cuisine",
                choices = sort(unique(cuChoice$`CUISINE DESCRIPTION`)),
                multiple = TRUE,
                selectize = TRUE,
                selected = unique(cuChoice$`CUISINE DESCRIPTION`)[1]
    )
  })
  
  #A value box showing the number of days since the most recent inspection
  output$TimeSinceInspection <- renderValueBox({
    res <- resInput()
    resCurrent <- res %>%
      filter(`INSPECTION DATE` == max(res$`INSPECTION DATE`))
    currentDate <- Sys.Date()
    lastInspect <- resCurrent$`INSPECTION DATE`
    days.since.last <- as.numeric(currentDate - lastInspect)
    valueBox(subtitle = "Days Since Last Inspection", value = days.since.last, icon = icon("calendar"), color = "green")
  })

  #A value box showing the Grade on the most recent inspection
  output$GradeLast <- renderValueBox({
    res <- resInput()
    resCurrent <- res %>%
      filter(`INSPECTION DATE` == max(res$`INSPECTION DATE`))
    valueBox(subtitle = "Grade on Last Inspection", value = resCurrent$GRADE, icon = icon("id-card-o"), color = "green")
  })
  
  #A value box showing the Violation score on the most recent inspection 
  output$ViolationCnt <- renderValueBox({
    res <- resInput()
    resCurrent <- res %>%
      filter(`INSPECTION DATE` == max(res$`INSPECTION DATE`))
    valueBox(subtitle = "Violation Score (Lower is Better)", value = resCurrent$SCORE, icon = icon("exclamation-triangle "), color = "green")
  })

  
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
    data1 <- data1 %>% filter(`INSPECTION DATE` >= input$dateRange1[1] & `INSPECTION DATE` <= input$dateRange1[2])
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
  
  #A plot of the number of critical violations per inspection date
  output$Viocrit <- renderPlotly({
    data1 <- resInput2()
    ggplotly(
      ggplot(data = data1, mapping = aes(x=`INSPECTION DATE`, y= crits, color=DBA))+
        geom_line() +
        labs(x="Inspection Dates", y="Number of Critical Violation"))
  })



  
  # Data table of Resturnat Inspections
  output$table <- DT::renderDataTable({
    subset(resInput(), select = c(DBA,`CUISINE DESCRIPTION`, `VIOLATION CODE`, SCORE, GRADE, `INSPECTION DATE`,`INSPECTION TYPE`))
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)