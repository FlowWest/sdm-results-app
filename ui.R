library(shiny)
library(shinythemes)
library(rsconnect)
library(shinycssloaders)

ui <- fluidPage(
  title = "DSM Results",
  titlePanel("DSM Results"),
  tabPanel("Results", 
           fluidRow(
             column(12, 
                    tabsetPanel(
                      tabPanel("Fall-Run",
                               column(5,
                                      dataTableOutput("fall_percent_change_table")), 
                               column(7, 
                                      tags$h3("Action Units"))), 
                      tabPanel("Spring-Run", 
                               column(5,
                                      dataTableOutput("spring_percent_change_table")), 
                               column(7, 
                                      tags$h3("Action Units"))),
                      tabPanel("Winter-Run", 
                               column(5,
                                      dataTableOutput("winter_percent_change_table")), 
                               column(7, 
                                      tags$h3("Action Units"))) 
                    ))
           ), 
           fluidRow()), 
  tabPanel("About")
  
)






