# Histograms Shiny App

library(shiny)
library(ggplot2)

df<-data.frame(x=rnorm(5,0,1))

shinyServer(function(input, output) {
  
  # Monitor 'Sample' button. When pressed, generate a random sample of normal data
  # based on selected sample size from slider input.
  
  df <- eventReactive(input$SampleButton, {
    data.frame(x=rnorm(input$n,0,1))
  }, ignoreNULL = FALSE)
  
  # Using random data, and slider input for number of histogram bins, generate 
  # histogram. Overlay theoretical and density estimate of distribution
  
  output$distPlot <- renderPlot({
    m <- ggplot(df(), aes(x=x))
    m + geom_histogram(bins=input$bins,aes(y = ..density..),colour = "white") + 
      geom_density(colour="blue") +
      stat_function(fun = dnorm, colour = "red") +
      scale_x_continuous(limits=c(-3,3)) + scale_y_continuous(limits=c(0,1))
  })
  
})
