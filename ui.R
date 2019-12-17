library(shiny)
library(shinythemes)
library(rsconnect)
library(shinycssloaders)


shinyUI <- navbarPage("DSM Results", fluid=TRUE, 
  tabPanel("Fall-Run",
          fluidPage(
            theme = shinythemes::shinytheme("readable"), 
            includeCSS("www/styles.css"),
            titlePanel("Fall-Run DSM Results", windowTitle = "Fall-Run DSM Results"),
            fluidRow(
              column(width = 4, 
                     tags$h3("Percent Change from No Actions"),
                     DT::dataTableOutput("percent_change_table")), 
              column(width = 8, 
                     tags$h3("Action Units"),
                     plotlyOutput("actions_plot"), 
                     tags$br(),
                     uiOutput("scenario_definition"),
                     tags$br())
            ), 
            fluidRow(
              column(
                width = 9,
                style = "margin-left: 20px;",
                tags$h3("Action Overview"),
                DT::dataTableOutput("actions_summary"))
            )
          )), 
        
  
  tabPanel("Spring-Run", 
           fluidPage(
             theme = shinythemes::shinytheme("readable"), 
             includeCSS("www/styles.css"),
             titlePanel("Spring-Run DSM Results", windowTitle = "Spring-Run DSM Results"),
             fluidRow(
               column(width = 4, 
                      tags$h3("Percent Change from No Actions"),
                      DT::dataTableOutput("percent_change_table")), 
               column(width = 8, 
                      tags$h3("Action Units"),
                      plotlyOutput("actions_plot"), 
                      tags$br(),
                      uiOutput("scenario_definition"),
                      tags$br())
             ), 
             fluidRow(
               column(
                 width = 9,
                 style = "margin-left: 20px;",
                 tags$h3("Action Overview"),
                 DT::dataTableOutput("actions_summary"))
             )
           )),
  
  
    tabPanel("Winter-Run",
             fluidPage(
               theme = shinythemes::shinytheme("readable"), 
               includeCSS("www/styles.css"),
               titlePanel("Winter-Run DSM Results", windowTitle = "Winter-Run DSM Results"),
               fluidRow(
                 column(width = 4, 
                        tags$h3("Percent Change from No Actions"),
                        DT::dataTableOutput("percent_change_table")), 
                 column(width = 8, 
                        tags$h3("Action Units"),
                        plotlyOutput("actions_plot"), 
                        tags$br(),
                        uiOutput("scenario_definition"),
                        tags$br())
               ), 
               fluidRow(
                 column(
                   width = 9,
                   style = "margin-left: 20px;",
                   tags$h3("Action Overview"),
                   DT::dataTableOutput("actions_summary"))
               )
             )),
  
    tabPanel("About",
               p(
                 "This Shiny App allows users to look at Fall, Spring, and Winter-Run DSM results."
# More text here                 
             
             ))
)
           
  
  
  
    
  
  