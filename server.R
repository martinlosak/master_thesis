library(shiny)
library(DT)
library(corrplot)
source("dbConnection.R")
source("script.R")
source("scriptCzechData.R")

shinyServer(function(input, output) {
  
  # Correlation
  observeEvent(input$correlationButton, {
    dbTable = input$dbTable
    if(input$dbTable == "sr_bratislava" || input$dbTable == "sr_zilina"|| input$dbTable == "sr_kosice"|| input$dbTable == "sr_poprad"){
      inputs = c("sun","temperature","pressure","wind","humidity","rainfall")
    } else {
      inputs = c("cloud","temperature","pressure","wind","humidity","dewpoint")
    }
    
    # 
    data = getValues(input$dateRange[1], input$dateRange[2], inputs, input$dbTable)
    # data = na.omit(data)
    data.corr = correlate(data, input$corrMethod)
    
    output$corrplot = renderPlot(
      corrplot(
        corr = data.corr,
        method = "color",
        addCoef.col = "black"
      ))
  })
  
  # Region Correlation
  observeEvent(input$regionCorrelationButton, {
    
    correlatedRegions = data.frame()
    
    for (keyName in input$keyNames){
      tableName = getTableName(keyName)
      data = getValues(input$regionDateRange[1], input$regionDateRange[2], input$variables, tableName)
      correlatedData = correlate(data, input$regionCorrMethod)
      correlatedData = data.frame(correlatedData)
      correlatedData$region = getRegion(keyName)
      correlatedData = correlatedData[1, 2:ncol(correlatedData)]
      correlatedRegions = rbind(correlatedRegions, correlatedData)
    }
    
    output$regionCorrelationPlot = renderPlot({regionCorrelationChart(correlatedRegions) })
  })
  
  # General Additive Models
  observeEvent(input$gamButton, {
    inputs = c("time","day","holiday","season","sun","temperature","pressure","wind","humidity","rainfall")
    data = getValues(input$gamDateRange[1], input$gamDateRange[2], inputs, "sr_bratislava")
    
    output$gamPlot = renderPlot({
      createGamModel(data, input$baseVariable, input$additionalVariable)
    })
  })
  
  ## NEURAL NETWORK ##
  observeEvent(input$nnButton, {
    data = getValues(input$nDateRange[1], input$nDateRange[2], input$nInputs, input$nDatabase)
    
    output$drawPlot = renderPlot({
      drawPlot(data)
    })
    
    observeEvent(input$nnModelButton, {
      library(plyr)
      split = round_any(input$split*nrow(data)/100, 96, floor)
      output$neural = renderPrint({
        nn <<- createNeuralModel(data, input$nInputs, split)
        computed <<- computeANN(nn, data, input$nInputs, split)
      })
      
      output$mapeBox = renderValueBox({
        valueBox(paste(computed$mape, "%", sep = " "), "MAPE", icon = icon("percent"), color = "red")
      })
      
      output$drawNNPlot = renderPlot({
        drawNNPlot(nn)
      })
      
      output$computedPlot = renderPlot({
        computedPlot(computed)
      })
      
      output$computedLinePlot = renderPlot({
        computedLinePlot(computed)
      })
      
      output$computedResidualsPlot = renderPlot({
        computedResidualsPlot(computed)
      })
    })
  })

  # TAB - ACTIVE ENERGY
  observeEvent(input$activeButton, {
    summary = consumptionBy(input$typeOfAggregation, input$clusteringType)

    conditionalStyle = NULL
    

    

    output$activeTable = renderDataTable(summary, options = list(rowCallback = DT::JS(conditionalStyle),
                                                                   paging = FALSE,
                                                                   searching = FALSE))
  })

  # TAB - REACTIVE ENERGY
  observeEvent(input$reactiveButton, {
    table = createSummary()
    conditionalStyle = 'function(row, data) {
      if (data[1] < 0.5) {
        $("td:eq(1)", row).css("color", "green");
      } else {
        $("td:eq(1)", row).css("color", "red");
      }
    }'

    output$reactiveTable = renderDataTable(table, selection = "single", options = list(rowCallback = DT::JS(conditionalStyle),
                                                                                       searching = FALSE))
    
    output$reactivePlot = renderPlot({
      selected = input$reactiveTable_rows_selected
      file = table[selected,]$Meter
      reactivePlot(file)
    })
  })
})
