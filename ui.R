# Histograms Shiny App

library(shiny)

shinyUI(fluidPage(
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
                  value = 5),
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
))
