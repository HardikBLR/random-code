library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Sample Size Determination for Population Mean"),
  
  sidebarLayout(
    sidebarPanel(
      
      numericInput("k", "Number of Subgroups:",
                   value = 4, min = 1),
      
      numericInput("bias", "Sampling Bias (proportion):",
                   value = 0.1, min = 0),
      
      numericInput("cost", "Expected Cost per Subgroup:",
                   value = 100, min = 1),
      
      numericInput("time", "Time per Subgroup (hours):",
                   value = 2, min = 0.1),
      
      actionButton("calc", "Calculate Sample Size")
    ),
    
    mainPanel(
      h4("Calculated Sample Size"),
      verbatimTextOutput("result"),
      
      h4("Design of the Experiment"),
      plotOutput("designPlot")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$calc, {
    
    # Fixed values for simplicity
    Z <- 1.96        # 95% confidence
    sigma <- 10      # assumed population SD
    E <- 2           # allowable error
    
    # Basic sample size
    n <- (Z^2 * sigma^2) / (E^2)
    
    # Adjusted sample size
    n_final <- (n * (1 + input$bias)) / input$k
    n_final <- ceiling(n_final)
    
    output$result <- renderText({
      paste("Required sample size per subgroup =", n_final)
    })
    
    # Data for experiment design plot
    design_data <- data.frame(
      Subgroup = paste("Group", 1:input$k),
      SampleSize = rep(n_final, input$k),
      Cost = rep(input$cost, input$k),
      Time = rep(input$time, input$k)
    )
    
    output$designPlot <- renderPlot({
      ggplot(design_data, aes(x = Subgroup, y = SampleSize)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = "Sample Size Allocation Across Subgroups",
             y = "Sample Size",
             x = "Subgroups") +
        theme_minimal()
    })
  })
}

shinyApp(ui = ui, server = server)
