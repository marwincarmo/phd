#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("My first app"),
    p("This is a new app"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          radioButtons("display_var",
                       "Which variable to display",
                       choices = c("Waiting time to next eruption" = "waiting",
                                   "Eruption time" = "eruptions"),
                       selected = "waiting"
          ),
          
            sliderInput("bins",
                        "Bins:",
                        min = 1,
                        max = 100,
                        value = 60)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # set x-axis label depending on the value of display_var
        if (input$display_var == "eruptions") {
          xlabel <- "Eruption Time (in minutes)"
        } else if (input$display_var == "waiting") {
          xlabel <- "Waiting Time to Next Eruption (in minutes)"
        }
        
        ggplot(faithful, aes(.data[[input$display_var]])) +
          geom_histogram(bins = input$bins,
                         fill = "steelblue3",
                         colour = "grey30") +
          xlab(xlabel) +
          theme_minimal()

        # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'steelblue3', border = 'grey30',
        #      xlab = 'Waiting time to next eruption (in mins)',
        #      main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
