library(shiny)
library(shinythemes)
library(shinydashboard)


header = dashboardHeader(title = "Disclosure of hidden relationships in the energy data")

sidebar = dashboardSidebar(sidebarMenu(
  menuItem(
    "Correlation",
    tabName = "tabCorrelation",
    icon = icon("bar-chart")
  ),
  menuItem(
    "Regional Correlation",
    tabName = "tabRegionalCorrelation",
    icon = icon("globe")
  ),
  menuItem("GAM", tabName = "tabGAM", icon = icon("line-chart")),
  menuItem("Neural network", tabName = "tabNeuralNetwork", icon = icon("cogs")),
  menuItem("Active energy", tabName = "tabActiveEnergy", icon = icon("bolt")),
  menuItem(
    "Reactive energy",
    tabName = "tabReactiveEnergy",
    icon = icon("bolt")
  )
))

body = dashboardBody(tabItems(
  # TAB - CORRELATION
  tabItem(tabName = "tabCorrelation",
          fluidRow(
            box(
              title = "Correlation plot",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              selectInput(
                inputId = "dbTable",
                label = "Choose region:",
                choices = list(
                  "Texas residential data" = "texas",
                  "Bratislava - industry data" = "sr_bratislava",
                  "Zilina - industry data" = "sr_zilina",
                  "Kosice - industry data" = "sr_kosice",
                  "Poprad - industry data" = "sr_poprad"
                )
              ),
              dateRangeInput(
                "dateRange",
                label = "Choose period:",
                start = "2014-07-01",
                end = "2014-07-31",
                format = "d.m.yyyy",
                weekstart = 1,
                separator = " to "
              ),
              selectInput(
                inputId = "corrMethod",
                label = "Choose correlation method:",
                choices = list("Spearman" = "spearman",
                               "Pearson" = "pearson")
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
              )
            )
          )),
  
  # TAB - REGIONAL CORRELATION
  tabItem(
    tabName = "tabRegionalCorrelation",
    fluidRow(
      box(
        title = "Regions comparison",
        width = 12,
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        
        
        fluidRow(
          column(
            4,
            checkboxGroupInput(
              inputId = "keyNames",
              label = "Choose regions:",
              choices = list(
                "Bratislava" = "BA",
                "Zilina" = "ZA",
                "Kosice" = "KE",
                "Poprad" = "PP"
              ),
              selected = c("BA", "ZA", "KE", "PP")
            )
          ),
          
          column(
            4,
            checkboxGroupInput(
              inputId = "variables",
              label = "Choose variables:",
              choices = list(
                "Sun" = "sun",
                "Temperature" = "temperature",
                "Pressure" = "pressure",
                "Wind" = "wind",
                "Humidity" = "humidity",
                "Rainfall" = "rainfall"
              ),
              selected = c("sun", "temperature", "pressure", "wind", "humidity", "rainfall")
            )
          ),
          column(
            4,
            dateRangeInput(
              "regionDateRange",
              label = "Choose period:",
              start = "2014-07-01",
              end = "2014-07-31",
              format = "d.m.yyyy",
              weekstart = 1,
              separator = " to "
            ),
            selectInput(
              "regionCorrMethod",
              "Choose correlation method:",
              choices = list("Spearman" = "spearman",
                             "Pearson" = "pearson"
                             )
            ),
            
            actionButton("regionCorrelationButton", "Analyze")
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.regionCorrelationButton > 0",
      box(
        title = "Regions correlation plot",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("regionCorrelationPlot")
      )
    )
  ),
  
  # TAB - GENERALIZED ADDITIVE MODELS
  tabItem(tabName = "tabGAM",
          fluidRow(
            box(
              title = "Setup",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              dateRangeInput(
                "gamDateRange",
                label = "Choose period:",
                start = "2014-07-01",
                end = "2014-07-31",
                min = "2013-08-01",
                max = "2015-02-16",
                format = "d.m.yyyy",
                weekstart = 1,
                separator = " to "
              ),
              fluidRow(column(
                6,
                radioButtons(
                  inputId = "baseVariable",
                  label = h3("Base variable"),
                  choices = list(
                    "Time" = "time",
                    "Day" = "day",
                    "Holiday" = "holiday",
                    "Season" = "season",
                    "Temperature" = "temperature",
                    "Pressure" = "pressure",
                    "Wind" = "wind",
                    "Humidity" = "humidity",
                    "Rainfall" = "rainfall",
                    "Sun" = "sun"
                  ),
                  selected = "day"
                )
              ),
              column(
                6,
                radioButtons(
                  inputId = "additionalVariable",
                  label = h3("Additional variable"),
                  choices = list(
                    "Time" = "time",
                    "Day" = "day",
                    "Holiday" = "holiday",
                    "Season" = "season",
                    "Temperature" = "temperature",
                    "Pressure" = "pressure",
                    "Wind" = "wind",
                    "Humidity" = "humidity",
                    "Rainfall" = "rainfall",
                    "Sun" = "sun"
                  ),
                  selected = "temperature"
                )
              )),
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
  
  # TAB - NEURAL NETWORK
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
          choices = list(
            "Texas residential data" = "texas",
            "Bratislava - industry data" = "sr_bratislava",
            "Zilina - industry data" = "sr_zilina",
            "Kosice - industry data" = "sr_kosice",
            "Poprad - industry data" = "sr_poprad"
          )
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
            "Load(w,d,h-1)" = "load_h1",
            "Load(w,d,h-2)" = "load_h2",
            "Load(w,d,h-3)" = "load_h3",
            "Load(w,d-1,h)" = "load_d1",
            "Load(w,d-1,h-1)" = "load_d1h1",
            "Load(w,d-1,h-2)" = "load_d1h2",
            "Load(w,d-1,h-3)" = "load_d1h3",
            "Load(w-1,d,h)" = "load_w1",
            "Time" = "time",
            "Day" = "day",
            "Holiday" = "holiday",
            "Season" = "season",
            "Temperature" = "temperature",
            "Pressure" = "pressure",
            "Wind" = "wind",
            "Humidity" = "humidity",
            "Rainfall" = "rainfall",
            "Sun (only SR)" = "sun",
            "Cloud (only TX)" = "cloud",
            "Dewpoint (only TX)" = "dewpoint"
          ),
          selected = c("load_d1", "load_w1", "time", "day", "holiday", "temperature", "humidity")
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
            min = 10,
            max = 100,
            value = 99,
            step = 1
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
  ),
  
  # TAB - ACTIVE ENERGY
  tabItem(tabName = "tabActiveEnergy",
          fluidRow(
            box(
              title = "Setup",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              selectInput(
                "typeOfAggregation",
                "Type of aggregation:",
                choices = list(
                  "Daily" = "day",
                  "Shiftly" = "shift",
                  "Hourly" = "hour"
                )
              ),
              radioButtons(
                inputId = "clusteringType",
                label = h3("Clustering type"),
                choices = list(
                  "K-means hourly trend clustering" = "trend",
                  "Company average load clustering" = "average"
                ),
                selected = "trend"
              ),
              actionButton("activeButton", "Analyze")
            ),
            conditionalPanel(
              condition = "input.activeButton > 0",
              box(
                title = "Table",
                width = 12,
                status = "success",
                solidHeader = TRUE,
                collapsible = TRUE,
                dataTableOutput("activeTable")
              )
            )
          )),
  
  # TAB - REACTIVE ENERGY
  tabItem(tabName = "tabReactiveEnergy",
          fluidRow(
            box(
              title = "Setup",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              actionButton("reactiveButton", "Analyze")
            ),
            conditionalPanel(
              condition = "input.reactiveButton > 0",
              box(
                title = "Plot",
                status = "success",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("reactivePlot")
              ),
              box(
                title = "Table",
                width = 12,
                status = "success",
                solidHeader = TRUE,
                collapsible = TRUE,
                dataTableOutput("reactiveTable")
              )
            )
          ))
))

dashboardPage(title = "Disclosure of hidden relationships in the energy data", skin = "green", header, sidebar, body)