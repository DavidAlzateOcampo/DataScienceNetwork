# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source("global.R")
library(shiny)

shinyServer<-function(input, output) {

  output$distPlot <- renderPlotly({
    varType<-get.Var.Type(input$dimension.1,data)
    data<-get.selected.data(input$survery)
  
    if(is.null(input$dimension.2)){
      if(varType == "integer"){
        plotly.Histogram.1.dim(input,data)
      }else if(varType == "factor"){
        plotly.Bar.1.dim(input,data)
      }
    }else{
      if(varType == "integer"){
        plotly.Histogram.1.dim(input,data)
      }else if(varType == "factor"){
        plotly.Bar.2.dim(input,data)
      }
    }
    

    
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2]
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  # Printing dimensions that were selected.
  output$dimension <- renderText(input$dimension)
  output$survey.year.final<- renderPrint({
      sur.yr <- input$survey.year
      paste(sur.yr,"100",sep="_")
    })
  
  output$var1<-renderText(input$dimension.1)
  output$dimensions.selectors.1 <- renderUI(
    dyn.print.selectors.1(input)
    )
  output$dimensions.selectors.2 <- renderUI(
    dyn.print.selectors.2(input)
  )
  output$dim.X.category.selectors<-renderUI(
    dyn.print.cat.X.select(input)
  )
  
  output$max.category.value <- renderInfoBox({
    infoBox(
      label.cat.Max.InfoBox
      , getRangeMax.Var.data(data,input$survery,input$dimension.1) 
      , icon = icon("arrow-up", lib = "glyphicon")
      , color = "red"
      , fill = TRUE
    )
  })
  
  output$max.category.perc.value <- renderInfoBox({
    infoBox(
      label.max.perc.InfoBox
      , getPercMax.Var.data(data,input$survery,input$dimension.1) 
      , icon = icon("percent")
      , color = "red"
      , fill = TRUE
    )})
  
  output$min.category.value <- renderInfoBox({
    infoBox(
      label.cat.Min.InfoBox
      , getRangeMin.Var.data(data,input$survery,input$dimension.1) 
      , icon = icon("arrow-down", lib = "glyphicon")
      , color = "green"
      , fill = TRUE
    )
  })
  
  output$min.category.perc.value <- renderInfoBox({
    infoBox(
      label.min.perc.InfoBox
      ,  getPercMin.Var.data(data,input$survery,input$dimension.1)
      , icon = icon("percent")
      ,color = "green"
      ,fill = TRUE
    )
  })
}

