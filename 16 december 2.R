library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Stratified Sampling: Sample Size Allocation"),
  
  sidebarLayout(
    sidebarPanel(
      
      numericInput("H", "Number of Strata:",
                   value = 3, min = 1),
      
      numericInput("bias", "Sampling Bias (proportion):",
                   value = 0.1, min = 0),
      
      numericInput("cost", "Expected Cost per Stratum:",
                   value = 100, min = 1),
      
      numericInput("time", "Time per Stratum (hours):",
                   value = 2, min = 0.1),
      
      actionButton("calc", "Calculate")
    ),
    
    mainPanel(
      h4("Sample Size Allocation"),
      tableOutput("allocationTable"),
      
      h4("Design of the Experiment"),
      plotOutput("designPlot")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$calc, {
    
    # Fixed parameters
    Z <- 1.96
    sigma <- 10
    E <- 2
    
    # Total sample size
    n <- (Z^2 * sigma^2) / (E^2)
    n_adj <- ceiling(n * (1 + input$bias))
    
    # Generate stratum details
    Nh <- sample(80:150, input$H, replace = TRUE)
    Sh <- runif(input$H, 5, 15)
    Ch <- rep(input$cost, input$H)
    
    # Proportional Allocation
    n_prop <- round(n_adj * Nh / sum(Nh))
    
    # Neyman Allocation
    n_neyman <- round(n_adj * (Nh * Sh) / sum(Nh * Sh))
    
    # Optimised Allocation
    n_opt <- round(n_adj * (Nh * Sh / sqrt(Ch)) /
                     sum(Nh * Sh / sqrt(Ch)))
    
    allocation <- data.frame(
      Stratum = paste("Stratum", 1:input$H),
      Population_Size = Nh,
      Proportional = n_prop,
      Neyman = n_neyman,
      Optimised = n_opt
    )
    
    output$allocationTable <- renderTable({
      allocation
    })
    
    output$designPlot <- renderPlot({
      ggplot(allocation, aes(x = Stratum)) +
        geom_bar(aes(y = Proportional, fill = "Proportional"),
                 stat = "identity", position = "dodge") +
        geom_bar(aes(y = Neyman, fill = "Neyman"),
                 stat = "identity", position = "dodge") +
        geom_bar(aes(y = Optimised, fill = "Optimised"),
                 stat = "identity", position = "dodge") +
        labs(title = "Sample Size Allocation Across Strata",
             y = "Sample Size",
             fill = "Allocation Method") +
        theme_minimal()
    })
  })
}

shinyApp(ui = ui, server = server)
