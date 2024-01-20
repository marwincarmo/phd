
library(shiny)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Interactive Simple Linear Regression"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("intercept", "Intercept:", min = -100, max = 200, value = 0),
      sliderInput("slope", "Slope:", min = -5, max = 5, value = 1),
      selectInput("xvar", "Choose X variable:", choices = names(mtcars)),
      selectInput("yvar", "Choose Y variable:", choices = names(mtcars)),
      checkboxInput("show_ci", "Show confidence interval", value = FALSE)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot <- renderPlot({
    data <- mtcars
    x <- data[[input$xvar]]
    y <- data[[input$yvar]]
    
    # Use user-defined intercept and slope
    intercept <- input$intercept
    slope <- input$slope
    
    ggplot(data, aes(x = x, y = y)) +
      geom_point() +
      geom_abline(intercept = intercept, slope = slope, color = "red") +  # Use user-defined line
      labs(x = input$xvar, y = input$yvar)
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
