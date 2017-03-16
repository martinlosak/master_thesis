library(shiny)
library(shinythemes)
library(shinydashboard)


header <- dashboardHeader(title = "Disclosure of hidden relationships in the energy data")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Correlation", tabName = "tabCorrelation", icon = icon("bar-chart")),
  menuItem("GAM", tabName = "tabGAM", icon = icon("bar-chart")),
  menuItem("Neural network", tabName = "tabNeuralNetwork", icon = icon("cogs"))
))

body <- dashboardBody(tabItems(
  # TAB 1
  tabItem(tabName = "tabCorrelation",
          fluidRow(
            box(
              title = "Setup",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              selectInput(
                "database",
                "Choose database:",
                choices = list("Slovak industry data" = "bratislava",
                               "Texas residential data" = "texas")
              ),
              dateRangeInput(
                "dateRange",
                label = "Choose period:",
                start = "2014-07-01",
                end = "2014-07-31",
                min = "2013-08-01",
                # max = "2015-02-16",
                format = "d.m.yyyy",
                weekstart = 1,
                separator = " to "
              ),
              selectInput(
                "corrMethod",
                "Choose correlation method:",
                choices = list("Pearson" = "pearson",
                               "Spearman" = "spearman")
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
              )
            )
          )),
  
  # TAB 2
  tabItem(tabName = "tabGAM",
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
              actionButton("gamButton", "Analyze")
            ),
            conditionalPanel(
              condition = "input.gamButton > 0",
              box(
                title = "GAM plot",
                width = 12,
                status = "success",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("gamPlot")
              )
            )
          )), 

  # TAB 3
  tabItem(
    tabName = "tabNeuralNetwork",
    fluidRow(
      box(
        title = "Setup",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        selectInput(
          "nDatabase",
          "Choose database:",
          choices = list("Slovak industry data" = "bratislava",
                         "Texas residential data" = "texas")
        ),
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
            "p(w,d,h-1)" = "load_h1",
            "p(w,d,h-2)" = "load_h2",
            "p(w,d,h-3)" = "load_h3",
            "p(w,d-1,h)" = "load_d1",
            "p(w,d-1,h-1)" = "load_d1h1",
            "p(w,d-1,h-2)" = "load_d1h2",
            "p(w,d-1,h-3)" = "load_d1h3",
            "p(w-1,d,h)" = "load_w1",
            "time" = "time",
            "day" = "day",
            "holiday" = "holiday",
            "season" = "season",
            "sun" = "sun",
            "temperature" = "temperature",
            "pressure" = "pressure",
            "wind" = "wind",
            "humidity" = "humidity",
            "rainfall" = "rainfall"
          ),
          selected = c("load_h1", "time", "day", "temperature")
        ),
        actionButton("nnButton", "Load data")
      ),
      
      # Po stlaceni tlacidla Load data
      conditionalPanel(
        condition = "input.nnButton > 0",
        box(
          title = "Real load",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("drawPlot"),
          sliderInput(
            "split",
            "Train / Test split:",
            min = 96,
            max = 2976,
            value = 2880,
            step = 96
          ),
          actionButton("nnModelButton", "Create model")
        )
      )
    ),

    # Po stlaceni tlacidla Create Model
    fluidRow(
      conditionalPanel(
        condition = "input.nnModelButton > 0",
            
        box(
          title = "Real vs Fitted load",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("computedLinePlot")
        ),
        box(
          title = "Residuals vs Fitted load",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("computedResidualsPlot")
        ),
        box(
          title = "Real vs Fitted load",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("computedPlot")
        ),
        box(
          title = "Model Creation",
          status = "danger",
          solidHeader = TRUE,
          collapsible = TRUE,
          verbatimTextOutput("neural")
        ),
        valueBoxOutput("mapeBox")
      )
    )
  )
))

dashboardPage(title="Disclosure of hidden relationships in the energy data", skin = "green", header, sidebar, body)