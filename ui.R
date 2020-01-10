ui <- fluidPage(
  title = "DSM Results",
  titlePanel("DSM Results"),
  fluidRow(
  column(width  = 4, 
         tags$h4("Percent Change from No Actions"),
         tabsetPanel(
           tabPanel(title = "Fall Run", DT::dataTableOutput("fall_run_percent_change")),
           tabPanel(title = "Spring Run", DT::dataTableOutput("spring_run_percent_change")),
           tabPanel(title = "Winter Run"), DT::dataTableOutput("winter_run_percent_change")
         )),
  column(width  = 4, 
         tags$h4("Actions"))
  ), 
  fluidRow()
  
)






