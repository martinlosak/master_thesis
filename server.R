library(shiny)
library(RMySQL)
library(corrplot)
source("script.R")

shinyServer(function(input, output) {
  
  ## CORRELATION TAB ##
  observeEvent(input$correlationButton, {
    inputs <- c("time","day","holiday","sun","temperature","pressure","wind","humidity","rainfall")
    data <- getValues(input$dateRange[1], input$dateRange[2], inputs)
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
  })

  ## GAM ##
  observeEvent(input$gamButton, {
    inputs <- c("time","day","holiday","sun","temperature","pressure","wind","humidity","rainfall")
    data <- getValues(input$nDateRange[1], input$nDateRange[2], inputs)
       
    output$gamPlot <- renderPlot({
      createGamModel(data)
    })
  })
  
  ## NEURAL NETWORK ##
  observeEvent(input$nnButton, {
    data <- getValues(input$nDateRange[1], input$nDateRange[2], input$nInputs)
       
    output$drawPlot <- renderPlot({
      drawPlot(data)
    })
    
    observeEvent(input$nnModelButton, {
      output$neural <- renderPrint({
        nn <<- createNeuralModel(data, input$nInputs, input$split)
        computed <<- computeANN(nn, data, input$nInputs, input$split)
      })

      output$mapeBox <- renderValueBox({
        valueBox(paste(computed$mape, "%", sep = " "), "MAPE", icon = icon("percent"), color = "red")
      })
      
      output$drawNNPlot <- renderPlot({
        drawNNPlot(nn)
      })
      
      output$computedPlot <- renderPlot({
        computedPlot(computed)
      })
      
      output$computedLinePlot <- renderPlot({
        computedLinePlot(computed)
      })
      
      output$computedResidualsPlot <- renderPlot({
        computedResidualsPlot(computed)
      })
    })
    
    
  })
  
})
