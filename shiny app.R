#Name: Mrunmai Prakash Musale    SUID: 259761726

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Exploring Old Faithful Eruption Intervals"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      # Add a descriptive paragraph about the app
      HTML("This interactive Shiny application allows you to explore the distribution of waiting times between eruptions of the Old Faithful geyser in Yellowstone National Park. Adjust the slider to change the number of bins in the histogram, which can help you visualize the data in different levels of granularity.")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # Displaying the total number of rows in the dataset
      textOutput("totalRows"),
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$totalRows <- renderText({
    # Calculating the total number of rows in the faithful dataset
    paste("Total number of observations:", nrow(faithful))
  })
  
  output$distPlot <- renderPlot({
    # generating bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col ='red', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


#PublishedURL-"https://c19dce762dbd4fdc9f28dbe3ed34aced.app.posit.cloud/p/62201352/"



