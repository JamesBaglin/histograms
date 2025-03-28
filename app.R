# Histograms Shiny App

library(shiny)

ui <- fluidPage(
  # App title
  titlePanel("Histograms - Exploring the Effect of Sample Size and Bin Number"),
  
  # Sidebar with a sample button and slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      p("Select a sample size to randomly sample. 
        Click sample to draw. If you draw multiple samples of the same sample size, 
        you can see how histograms can vary from sample to sample."),
      sliderInput("n",
                  "Sample size:",
                  min = 10,
                  max = 10000,
                  value = 10),
      actionButton("SampleButton", "Sample"),
      p("Adjust the number of bins to see how it impacts the histogram's appearance."),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 100,
                  value = 30)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      p("The red line is the target population distribution and the blue line is 
        the sample density estimate."),
      plotOutput("distPlot"),
      p("Histograms perform best for large samples that can support many bins.")
    )
  )
)

df<-data.frame(x=rnorm(5,0,1))

server <- function(input,output) {
  
  # Monitor 'Sample' button. When pressed, generate a random sample of normal data
  # based on selected sample size from slider input.
  
  df <- eventReactive(input$SampleButton, {
    data.frame(x=rnorm(input$n,0,1))
  }, ignoreNULL = FALSE)
  
  # Using random data, and slider input for number of histogram bins, generate 
  # histogram. Overlay theoretical and density estimate of distribution
  
  output$distPlot <- renderPlot({
    m <- ggplot(df(), aes(x=x))
    m + geom_histogram(bins=input$bins,aes(y = after_stat(density)), colour = "white") + 
      geom_density(colour="blue") +
      stat_function(fun = dnorm, colour = "red") +
      scale_x_continuous(limits=c(-3,3)) + scale_y_continuous(limits=c(0,1))
  })
  
}

shinyApp(ui = ui, server = server)