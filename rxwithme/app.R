library(shiny)
library(lubridate)
library(shinyjs)
library(bslib)
library(tidyverse)
library(shinyBS)


# Mock Database for ids/password, drug data and added_estimated time
registeredIds <- c("rxwithme" = "12345")
first_processed_time <- "09:35AM EST"
rx_queue <- 46
rx_status <- "Filling"
pharm_status <- "Maximum Capacity"


ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(
    version = 4,
    bootswatch = "flatly",
    font_scale = 1.0
  ),
  tags$head(tags$style(
    HTML(".shiny-notification {
             position:fixed;
             top: 0px;
             right: 0px;
             }
             ")
  )),
  tags$head(tags$style(
    HTML("#estimated_process{
             font-size: 25px;
             font-weight: bold;
             }
             ")
  )),
  tags$head(tags$style(
    HTML("#pharm_status_header{
             font-size: 25px;
             font-weight: bold;
             }
             ")
  )),
  tags$head(tags$style(
    HTML("#curr_meds{
             font-size: 25px;
             font-weight: bold;
             }
             ")
  )),
  tags$head(tags$style(
    HTML("#total_number{
             font-size: 20px;
             color: #F6BE00;
             }
             ")
  )),
  tags$head(tags$style(
    HTML("#first_processed{
             font-size: 20px;
             }
             ")
  )),
  tags$head(tags$style(
    HTML("#estimated_time{
             font-size: 20px;
             }
             ")
  )),
  tags$head(tags$style(
    HTML("#rx_queue{
             font-size: 20px;
             }
             ")
  )),
  tags$head(tags$style(
    HTML("#rx_status{
             font-size: 20px;
             }
             ")
  )),
  tags$head(tags$style(
    HTML("#pharm_status{
             color: red;
             font-size: 20px;
             font-style: bold;
             }
             ")
  )),
  tags$head(tags$style(
    HTML("#drug1{
             font-size: 20px;
             }
             ")
  )),
  tags$head(tags$style(
    HTML("#drug2{
             font-size: 20px;
             }
             ")
  )),
  br(),
  img(src = "rxtracker-logo.png", height = "70px", width = "275px"),
  fluidRow(
    column(
      4,
      br(),
      wellPanel(
        textInput("id",
                  "Phone/Email"),
        textInput("security_code", "Security Code"),
        actionButton("login", "Login")
      )
    ),
    column(8,
           div(id = "main",
           tabsetPanel(
             id = "main_tab",
             tabPanel(
               "Main Dashboard",
               hr(),
               textOutput("estimated_process"),
               textOutput("first_processed"),
               textOutput("estimated_time"),
               textOutput("rx_queue"),
               textOutput("rx_status"),
               br(),
               textOutput("pharm_status_header"),
               textOutput("pharm_status"),
               br(),
               textOutput("curr_meds"),
               textOutput("total_number"),
               br(),
               textOutput("drug1"),
               textOutput("drug2")
             )
           )) %>% shinyjs::hidden())
  )
)


server <- function(input, output, session) {
  #Static estimated time in seconds due to time contraints
  estimated_time <- reactiveVal(630)
  finished <- reactiveVal(FALSE)
  added <- reactiveVal(FALSE)
    
  observeEvent(input$login, {
    if (input$id %in% names(registeredIds) && input$security_code %in% registeredIds[input$id]) {
      shinyjs::toggle("main")
    } else {
      showNotification("Invalid email/phone or security code", type = 'err')
    }
  })
  output$estimated_time<- renderText(paste("Estimated Time: ", seconds_to_period(estimated_time())))
  observe({
    invalidateLater(1000, session)
    isolate({
      if (!finished()){
        estimated_time(estimated_time()-1)
        if (!added() && estimated_time() < 600) {
          estimated_time(estimated_time() + 900)
          added(TRUE)
        }
        if(estimated_time() < 1)
        {
          finished(TRUE)
          estimated_time("Finished")
          rx_status("Filled!")
        }
      } 
    })
  })
  
  output$estimated_process <- renderText("Estimated Process")
  output$first_processed <- renderText(paste("First Processed Time: ", first_processed_time))
  output$rx_queue<- renderText(paste("Rx In Queue: ", rx_queue))
  output$rx_status<- renderText(paste("Rx Status: ", rx_status))
  output$pharm_status_header <- renderText("Pharmacy Status")
  output$pharm_status <- renderText(paste("Pharmacy Status:", pharm_status))
  output$curr_meds <- renderText("Current Medications")
  output$total_number <- renderText("Total Number: 2")
  output$drug1 <- renderText({ c("Amoxicillin 500 mg: 10 days, 0 refill") })
  output$drug2 <- renderText("Salofalk 500 mg: 7 days, 0 refill")
  
  
}

shinyApp(ui, server)
