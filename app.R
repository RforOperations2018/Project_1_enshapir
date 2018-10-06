# ----------------------
#enshapir
# 
# project 1 updated for HW 4
#-----------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(jsonlite)
library(plotly)
library(DT)
library(shinythemes)
library(httr)
library(htmltools)



#DOHMH New York City Restaurant Inspection Results
#https://data.cityofnewyork.us/Health/DOHMH-New-York-City-Restaurant-Inspection-Results/43nn-pn8j


#URL to pull out just the names and uniquie ids along with just the resturants I want to focus on
url <- paste0("https://data.cityofnewyork.us/resource/9w7m-hzhe.json",'?',"$select=camis, dba, cuisine_description","&","$where=camis in('41615257','40813994','40685734','50048821','50003527','41561808','40824179','40388091','50066109','41241757','40918579','41365100')",'&$limit=10000')

r <- RETRY("GET", url = URLencode(url))
# Extract Content
c <- content(r, "text")
# Basic gsub to make NA's consistent with R
json <- gsub('NaN', 'NA', c, perl = TRUE)

# FYI there is also a package called RSocrata that will let you make API calls to the NYC a bit easier. Also, technically I believe you need at oken for any type of request which is over 1000. Limiting should handle this a bit though

#loading in resturant data
Resturant.load <- data.frame(fromJSON(json))

# Define dashboard UI
header <- dashboardHeader(title = 'Restaurant Inspections')

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    selectizeInput(inputId = "selectResturant", 
                   label = "Resturant:", 
                   choices = sort(unique(Resturant.load$dba)), 
                   selected = sort(unique(Resturant.load$dba)[1]), 
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
    #get unique id to query with
    camis.code <- unique(Resturant.load$camis[Resturant.load$dba == input$selectResturant])
  #create url to get data
    url <- paste0("https://data.cityofnewyork.us/resource/9w7m-hzhe.json",'?',"$where=camis in('", camis.code,"')",'&$limit=10000')
    
    r <- RETRY("GET", url = URLencode(url))
    # Extract Content
    c <- content(r, "text")
    # Basic gsub to make NA's consistent with R
    json <- gsub('NaN', 'NA', c, perl = TRUE)
    # Create Dataframe
    Resturant.data1 <- data.frame(fromJSON(json))
    
    Resturant.data1$inspection_date <- as.Date(Resturant.data1$inspection_date)
    
    return(Resturant.data1)
  })
  
  #creating a dataset of restaurants selected by cuisine type
  cuInput <- reactive({
    #create url to get data
    url <- paste0("https://data.cityofnewyork.us/resource/9w7m-hzhe.json?$select=inspection_date, avg(score) AS avg_score, cuisine_description&$group=inspection_date, cuisine_description&$where=cuisine_description in('Ice Cream, Gelato, Yogurt, Ices','American','Italian','Ethiopian','Indian','Pizza','Thai','Chinese','Japanese','Middle Eastern')&$limit=1000")
    #get the api data
    r <- RETRY("GET", url = URLencode(url))
    # Extract Content
    c <- content(r, "text")
    # Basic gsub to make NA's consistent with R
    json <- gsub('NaN', 'NA', c, perl = TRUE)
    # Create Dataframe
    Resturant.data2 <- data.frame(fromJSON(json)) %>% 
      filter(cuisine_description == input$selectCuis | cuisine_description ==resInput()$cuisine_description)
   
     Resturant.data2$inspection_date <- as.Date(Resturant.data2$inspection_date)
     
    return(Resturant.data2)
  })
  
  #creating a dataset with counts of critical inputs per resturant
  resInput2 <- reactive({
      #get unique id to query with
      #create url to get data
      url <- paste0("https://data.cityofnewyork.us/resource/9w7m-hzhe.json",'?$select=dba, inspection_date, score, count(critical_flag) AS crits&$group=dba, inspection_date, score',"&$where=camis in('41615257','40813994','40685734','50048821','50003527','41561808','40824179','40388091','50066109','41241757','40918579','41365100')AND critical_flag='Critical'",'&$limit=10000')
      r <- RETRY("GET", url = URLencode(url))
      # Extract Content
      c <- content(r, "text")
      # Basic gsub to make NA's consistent with R
      json <- gsub('NaN', 'NA', c, perl = TRUE)
      # Create Dataframe
      # This works, but isn't necessarily the way I would have done it.
      Resturant.data3 <- data.frame(fromJSON(json)) %>% 
        filter(dba %in% input$streetComp | dba %in% resInput()$dba)
      
      Resturant.data3$inspection_date <- as.Date(Resturant.data3$inspection_date)
      
      return(Resturant.data3)
      
  })
  
  #A date range selection for the currently selected restaurant’s inspection dates
  output$dateRange <- renderUI({
    dateRangeInput(inputId = "dateRange1",
                   label = "Pick a Date Range",
                   start = min(resInput()$inspection_date, na.rm = TRUE), 
                   end = max(resInput()$inspection_date, na.rm = TRUE),
                   min = min(resInput()$inspection_date, na.rm = TRUE),
                   max = max(resInput()$inspection_date, na.rm = TRUE)
                   )
    })
  
  #A selection box for resturant that does not show the currently selected restaurant as an option
  output$streetComp1 <- renderUI({
    resChoice <-  Resturant.load %>% filter(dba != resInput()$dba)
    
    selectInput(inputId = "streetComp",
                   label = "Resturant Comparison",
                   choices = sort(unique(resChoice$dba)),
                   multiple = TRUE,
                   selectize = TRUE,
                   selected = unique(resChoice$dba)[1]
                   )
  })
  
  #A selection box for cuisine type that does not show the currently selected restaurant’s cuisine as an option
  output$selectCuis1 <- renderUI({
    cuChoice <-  Resturant.load %>% filter(cuisine_description != resInput()$cuisine_description)
    
    selectInput(inputId = "selectCuis",
                label = "Cuisine",
                choices = sort(unique(cuChoice$cuisine_description)),
                multiple = TRUE,
                selectize = TRUE,
                selected = unique(cuChoice$cuisine_description)[1]
    )
  })
  
  #A value box showing the number of days since the most recent inspection
  output$TimeSinceInspection <- renderValueBox({
    res <- resInput()
    resCurrent <- res %>%
      filter(inspection_date == max(res$inspection_date))
    currentDate <- Sys.Date()
    lastInspect <- resCurrent$inspection_date
    days.since.last <- as.numeric(currentDate - lastInspect)
    valueBox(subtitle = "Days Since Last Inspection", value = days.since.last, icon = icon("calendar"), color = "green")
  })

  #A value box showing the Grade on the most recent inspection
  output$GradeLast <- renderValueBox({
    res <- resInput()
    resCurrent <- res %>%
      filter(inspection_date == max(res$inspection_date))
    valueBox(subtitle = "Grade on Last Inspection", value = resCurrent$grade, icon = icon("id-card-o"), color = "green")
  })
  
  #A value box showing the Violation score on the most recent inspection 
  output$ViolationCnt <- renderValueBox({
    res <- resInput()
    resCurrent <- res %>%
      filter(inspection_date == max(res$inspection_date))
    # How low of a score?
    valueBox(subtitle = "Violation Score (Lower is Better)", value = resCurrent$score,
             icon = icon("exclamation-triangle "), color = "green")
  })

  
  # Data table of Resturnat Inspections
  output$vioTable <- DT::renderDataTable({
    res <- resInput()
    resCurrent <- res %>%
      subset(inspection_date == max(res$inspection_date))
    subset(resCurrent, select = c(violation_code,violation_description))
  })
  
  # A plot showing the violations overtime of the resturant
  output$ViolationsOverTime <- renderPlotly({
    data1 <- resInput()
    data1 <- data1 %>% filter(inspection_date >= input$dateRange1[1] & inspection_date <= input$dateRange1[2])
    ggplotly(
      ggplot(data = data1, mapping = aes(x=inspection_date, y=as.numeric(as.character(score))))+
      geom_line()+
      labs(x="Inspection Dates", y="Violation Score"))
  })
  
  # A plot showing the violations overtime of the resturant
  output$VioCuisine <- renderPlotly({
    data1 <- cuInput()
    ggplotly(
      ggplot(data = data1, mapping = aes(x=inspection_date, y= as.numeric(as.character(avg_score)), color=cuisine_description))+
        geom_line() +
        labs(x="Inspection Dates", y="Violation Score"))
  })
  
  #A plot of the number of critical violations per inspection date
  output$Viocrit <- renderPlotly({
    data1 <- resInput2()
    ggplotly(
      ggplot(data = data1, mapping = aes(x=inspection_date, y=as.numeric(as.character(crits)), color=dba))+
        geom_line() +
        labs(x="Inspection Dates", y="Number of Critical Violation"))
  })



  
  # Data table of Resturnat Inspections
  output$table <- DT::renderDataTable({
    subset(resInput(), select = c(dba,cuisine_description, score, grade, inspection_date, inspection_type))
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)