library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("superhero"),
  navbarPage(
    "Disclosure of hidden relationships in the energy data",
    tabPanel(
      "Pearson correlation",
      sidebarLayout(
        sidebarPanel(
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
        
        mainPanel(
          textOutput("dateRangeText"),
          plotOutput("corrplot"),
          br(),
          tableOutput("corr"),
          verbatimTextOutput("debug")
        )
      )
    )
    ,
    tabPanel("Neural network",
             sidebarLayout(
               sidebarPanel(
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
                   label = h3("Choose inputs to ANN:"),
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
                 actionButton("nButton", "Analyze")
                 
               )
               ,
               
               mainPanel(
                 textOutput("nDateRangeText"),
                 br(),
                 plotOutput("drawPlot"),
                 br()
                 ,
                 conditionalPanel(
                   condition = "input.nButton > 0",
                   sliderInput(
                     "split",
                     "Train/Test split:",
                     min = 96,
                     max = nrow(data),
                     value = 2880,
                     step = 96
                   ),
                   actionButton("mButton", "Create ANN model")
                 )
                 ,
                 conditionalPanel(
                   condition = "input.mButton > 0",
                   verbatimTextOutput("neural"),
                   plotOutput("computedPlot"),
                   plotOutput("computedLinePlot"),
                   plotOutput("drawNNPlot")
                 )
                 
               )
             ))
  )
))
