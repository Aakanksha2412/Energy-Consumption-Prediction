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
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Energy Consumption Predictions_Group6"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_city", "Select a City:", choices = unique(filtered_df_1$in_city)),
      selectInput("selected_hvac_type", "Select HVAC Cooling Type:", choices = unique(filtered_df_1$in_hvac_cooling_type)),
      sliderInput("temp_range", "Select Temperature Range:", 
                  min = min(filtered_df_1$dry_bulb_temperature_c, na.rm = TRUE), 
                  max = max(filtered_df_1$dry_bulb_temperature_c, na.rm = TRUE), 
                  value = c(min(filtered_df_1$dry_bulb_temperature_c, na.rm = TRUE), max(filtered_df_1$dry_bulb_temperature_c, na.rm = TRUE))),
      selectInput("selected_ducts", "Select Ducts Condition:", choices = unique(filtered_df_1$in_ducts)),
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Previous fileInput code
      numericInput('nRows', 'Number of rows to display:', 5),

      ),
    mainPanel(
      plotOutput("predictionPlot"),
      helpText("Data from the uploaded file"),
      tableOutput('contents'),
      
      # model outputs
      helpText("The first 10 rows of predicted energy usage by linear model."),
      tableOutput('predictionResults'),
      helpText("Confusion Matrix based on logistic regression model (1:over than 1.0 kWh, 0:lower than 1.0 kWh)",
               "High numbers (predicted/actual is 0/0 or 1/1) indicate a good predictive model."),
      tableOutput('confMatrix')
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    filtered_df_1 %>%
      filter(in_city == input$selected_city, 
             in_hvac_cooling_type == input$selected_hvac_type,
             dry_bulb_temperature_c >= input$temp_range[1],
             dry_bulb_temperature_c <= input$temp_range[2],
             in_ducts == input$selected_ducts)
  })
  
  output$predictionPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = hour, y = predictions, color = in_city)) +
      geom_line() +
      labs(title = "Predictions vs. Hour of Day",
           x = "Hour of the Day",
           y = "Energy Consumption Predictions (Units)",
           color = "City") +
      ylim(0,3)+
      theme_minimal()
  })
  
  output$contents <- renderTable({
    req(input$file1)
    read.csv(input$file1$datapath, nrows = input$nRows) # You can replace this with read functions for different file types
  })
  
  
  data <- reactive({
    df <- read.csv(input$file1$datapath)
    
    # convert some changed type to the original type
    df$hour <- factor(sprintf("%02d", as.numeric(df$hour)))
    df <- df %>%
      mutate(across(where(is.character), as.factor))
    df <- df %>%
      mutate(hour = factor(hour),
             in_occupants = factor(in_occupants))
    return(df)
  })
  
  # generate predictions
  model_1 <- lm_train1
  predictions <- reactive({
    req(data())
    exp(predict(model_1, newdata = data()))
  })
  
  # display the results
  output$predictionResults <- renderTable({
    req(data())
    df <- data()
    df$predictions <- predictions()  # add predictions
    df$time <- as.POSIXct(df$time, format = "%Y-%m-%d %H:%M:%S")
    df$time <- format(df$time, "%m-%d %H:%M:%S")
    df$time <- as.factor(df$time)
    # select the columns to be displayed
    df <- dplyr::select(df, time, in_city, predictions)  
    return(head(df, 10))
  })
  
  model_2 <- model_glm
  predictions_glm <- reactive({
    req(data())
    predict(model_2, newdata = data())
  })
  
  output$confMatrix <- renderTable({
    req(data())
    df2 <- data()
    df2$`over 1.0 kWh` <- ifelse(df2$total_energy_consumption > 1.0, 1, 0) 
    actual <- df2$`over 1.0 kWh`
    predicted <- predictions_glm()
    # classify based on predictions
    predicted <- ifelse(predicted > 0.5, 1, 0)
    
    # Evaluate the model
    conf_matrix <- table(predicted, actual)
    return(conf_matrix)
  })
}

# Run
shinyApp(ui, server)
