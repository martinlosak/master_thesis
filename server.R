library(shiny)
library(RMySQL)
library(corrplot)
source("script.R")

shinyServer(function(input, output) {
  
  ## CORRELATION TAB ##
  observeEvent(input$correlationButton, {
    data <- getCorrelationValues(input$dateRange[1], input$dateRange[2])
    # data.corr <- cor(data[sapply(data, is.numeric)])
    data.corr <- correlate(data,input$corrMethod)
    
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
  
  observeEvent(input$nnButton, {
    data <- getValues(input$nDateRange[1], input$nDateRange[2], input$nInputs)
    
    output$debuger <- renderPrint({
      input$nInputs
    })
  
    output$drawPlot <- renderPlot({
      drawPlot(data)
    })

    output$neural <- renderPrint({
      nn <<- createNeuralModel(data, input$nInputs)
    })
    
    output$drawNNPlot <- renderPlot({
      plot(nn, intercept=FALSE, show.weights = FALSE, col.entry="red", col.hidden = "blue", col.out = "green")
    })
    
    output$computedPlot <- renderPlot({
      computed <- computeANN(nn,data, input$nInputs)
      computedPlot(computed)
    })
    
    output$computedLinePlot <- renderPlot({
      computed <- computeANN(nn,data, input$nInputs)
      computedLinePlot(computed)
    })
    
    output$computedResidualsPlot <- renderPlot({
      computed <- computeANN(nn,data, input$nInputs)
      plot <- computedResidualsPlot(computed)
      print(plot)
    })
  })
  
})
