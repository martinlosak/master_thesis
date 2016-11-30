# library(shiny)
# library(shinythemes)
# 
# shinyUI(fluidPage(
#   theme = shinytheme("superhero"),
#   navbarPage(
#     "Disclosure of hidden relationships in the energy data",
#     tabPanel(
#       "Pearson correlation",
#       sidebarLayout(
#         sidebarPanel(
#           dateRangeInput(
#             "dateRange",
#             label = "Choose period:",
#             start = "2014-07-01",
#             end = "2014-07-31",
#             min = "2013-08-01",
#             max = "2015-02-16",
#             format = "d.m.yyyy",
#             weekstart = 1,
#             separator = " to "
#           ),
#           actionButton("correlationButton", "Analyze")
#         ),
#         
#         mainPanel(
#           textOutput("dateRangeText"),
#           plotOutput("corrplot"),
#           br(),
#           tableOutput("corr"),
#           verbatimTextOutput("debug")
#         )
#       )
#     )
#     ,
#     tabPanel("Neural network",
#              sidebarLayout(
#                sidebarPanel(
#                  dateRangeInput(
#                    "nDateRange",
#                    label = "Choose period:",
#                    start = "2014-07-01",
#                    end = "2014-07-31",
#                    min = "2013-08-01",
#                    max = "2015-02-16",
#                    format = "d.m.yyyy",
#                    weekstart = 1,
#                    separator = " to "
#                  ),
#                  
#                  checkboxGroupInput(
#                    "nInputs",
#                    label = h3("Choose inputs to ANN:"),
#                    choices = list(
#                      "p(w,d,h)" = "load",
#                      "p(w,d,h-1)" = "load_h1",
#                      "p(w,d,h-2)" = "load_h2",
#                      "p(w,d,h-3)" = "load_h3",
#                      "time" = "time",
#                      "day" = "day",
#                      "holiday" = "holiday",
#                      "sun" = "sun",
#                      "temperature" = "temperature",
#                      "pressure" = "pressure",
#                      "wind" = "wind",
#                      "humidity" = "humidity",
#                      "rainfall" = "rainfall"
#                    ),
#                    selected = c("load", "temperature")
#                  ),
#                  actionButton("nButton", "Analyze")
#                  
#                )
#                ,
#                
#                mainPanel(
#                  textOutput("nDateRangeText"),
#                  br(),
#                  plotOutput("drawPlot"),
#                  br()
#                  ,
#                  conditionalPanel(
#                    condition = "input.nButton > 0",
#                    sliderInput(
#                      "split",
#                      "Train/Test split:",
#                      min = 96,
#                      max = nrow(data),
#                      value = 2880,
#                      step = 96
#                    ),
#                    actionButton("mButton", "Create ANN model")
#                  )
#                  ,
#                  conditionalPanel(
#                    condition = "input.mButton > 0",
#                    verbatimTextOutput("neural"),
#                    plotOutput("computedPlot"),
#                    plotOutput("computedLinePlot"),
#                    plotOutput("drawNNPlot")
#                  )
#                  
#                )
#              ))
#   )
# ))


library(shiny)
library(shinythemes)
library(shinydashboard)


header <- dashboardHeader(title = "Disclosure of hidden relationships in the energy data")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Pearson correlation", tabName = "pearsonCorrelation", icon = icon("bar-chart")),
  menuItem("Neural network", tabName = "neuralNetwork", icon = icon("cogs"))
))

body <- dashboardBody(tabItems(
  # TAB 1
  tabItem(tabName = "pearsonCorrelation",
          fluidRow(
            box(
              title = "Setup",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              dateRangeInput(
                "dateRange",
                label = "Choose period:",
                start = "2014-07-01",
                end = "2014-07-31",
                min = "2013-08-01",
                max = "2015-02-16",
                format = "d.m.yyyy",
                weekstart = 1,
                separator = " to "
              ),
              actionButton("correlationButton", "Analyze")
            ),
            conditionalPanel(
              condition = "input.correlationButton > 0",
              box(
                title = "Correlation plot",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("corrplot")
              ),
              box(
                title = "Correlation Table",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,
                tableOutput("corr")
              ),
              box(
                title = "Debug",
                status = "danger",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,
                verbatimTextOutput("debug")
              )
            )
          )),
  
  # TAB 2
  tabItem(tabName = "neuralNetwork",
          fluidRow(
            
            box(
              title = "Setup",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              dateRangeInput(
                "nDateRange",
                label = "Choose period:",
                start = "2014-07-01",
                end = "2014-07-31",
                min = "2013-08-01",
                max = "2015-02-16",
                format = "d.m.yyyy",
                weekstart = 1,
                separator = " to "
              ),
              checkboxGroupInput(
                "nInputs",
                label = "Choose inputs to ANN:",
                choices = list(
                  "p(w,d,h)" = "load",
                  "p(w,d,h-1)" = "load_h1",
                  "p(w,d,h-2)" = "load_h2",
                  "p(w,d,h-3)" = "load_h3",
                  "time" = "time",
                  "day" = "day",
                  "holiday" = "holiday",
                  "sun" = "sun",
                  "temperature" = "temperature",
                  "pressure" = "pressure",
                  "wind" = "wind",
                  "humidity" = "humidity",
                  "rainfall" = "rainfall"
                ),
                selected = c("load", "temperature")
              ),
              actionButton("nnButton", "Analyze")
            ),
            
            #input period selection
            conditionalPanel(
              condition = "input.nnButton > 0",
              box(
                title = "Plot",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("drawPlot")
              ),
              box(
                title = "Model setup",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                sliderInput(
                  "split",
                  "Train/Test split:",
                  min = 96,
                  max = 2976,
                  value = 2880,
                  step = 96
                ),
                actionButton("nnModelButton", "Create ANN model")
              )
            ),
            
            # model creation
            conditionalPanel(
              condition = "input.nnModelButton > 0",
              
              box(
                title = "ANN Model creation",
                status = "danger",
                solidHeader = TRUE,
                collapsible = TRUE,
                verbatimTextOutput("neural")
              ),
              
              tabBox(
                title = icon("line-chart"),
                tabPanel("Regression", plotOutput("computedPlot")),
                tabPanel("Line", plotOutput("computedLinePlot"))
              )
            )
            
          ))
))

dashboardPage(skin = "green", header, sidebar, body)