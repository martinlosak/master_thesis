library(shiny)
library(RMySQL)
library(corrplot)
source("script.R")

shinyServer(function(input, output) {
  observeEvent(input$correlationButton, {
    # right after button click
    data <- getValues(input$dateRange[1], input$dateRange[2])
    data.corr <- cor(data[sapply(data, is.numeric)])
    
    output$dateRangeText  <- renderText({
      paste("Period:", input$dateRange[1], "to", input$dateRange[2])
    })
    
    output$corrplot <- renderPlot({
      corrplot(
        data.corr,
        method = "color",
        addCoef.col = "black"
      )
    })
    
    output$corr <- renderTable({
      data.corr
    },
    striped = TRUE,
    bordered = TRUE,
    hover = TRUE,
    rownames = TRUE,
    digits = 2)
    
    output$debug <- renderPrint({
      head(data)
    })
    
  })
  
  observeEvent(input$nButton, {
    # right after button click
    data <- getValues(input$nDateRange[1], input$nDateRange[2])

    output$nDateRangeText  <- renderText({
      paste("Period:", input$nDateRange[1], "to", input$nDateRange[2])
    })
    
    output$drawPlot <- renderPlot({
      drawPlot(data)
    })

    output$neural <- renderPrint({
      nn <<- createNeuralModel(data)
    })
    
    output$drawNNPlot <- renderPlot({
      plot(nn, intercept=FALSE, show.weights = FALSE, col.entry="red", col.hidden = "blue", col.out = "green")
    })
    
    output$computedPlot <- renderPlot({
      computed <- computeANN(nn,data)
      computedPlot(computed)
    })
    
    output$computedLinePlot <- renderPlot({
      computed <- computeANN(nn,data)
      computedLinePlot(computed)
    })
  })
  
})
