library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Cluster Sampling: Sample Size Determination"),
  
  sidebarLayout(
    sidebarPanel(
      
      numericInput("cluster_size",
                   "Average Cluster Size:",
                   value = 20, min = 1),
      
      numericInput("bias",
                   "Sampling Bias (proportion):",
                   value = 0.1, min = 0),
      
      numericInput("cost",
                   "Expected Cost per Cluster:",
                   value = 500, min = 1),
      
      numericInput("time",
                   "Time per Cluster (hours):",
                   value = 3, min = 0.1),
      
      actionButton("calc", "Calculate")
    ),
    
    mainPanel(
      h4("Calculated Number of Clusters"),
      verbatimTextOutput("result"),
      
      h4("Design of the Experiment"),
      plotOutput("designPlot")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$calc, {
    
    # Fixed parameters
    Z <- 1.96        # 95% confidence
    sigma <- 12      # assumed SD
    E <- 3           # allowable error
    
    # Total sample size
    n <- (Z^2 * sigma^2) / (E^2)
    n_adj <- n * (1 + input$bias)
    
    # Required number of clusters
    clusters <- ceiling(n_adj / input$cluster_size)
    
    output$result <- renderText({
      paste("Required number of clusters =", clusters)
    })
    
    # Design data
    design_data <- data.frame(
      Cluster = paste("Cluster", 1:clusters),
      SampleUnits = rep(input$cluster_size, clusters),
      Cost = rep(input$cost, clusters),
      Time = rep(input$time, clusters)
    )
    
    output$designPlot <- renderPlot({
      ggplot(design_data, aes(x = Cluster, y = SampleUnits)) +
        geom_bar(stat = "identity", fill = "darkgreen") +
        labs(title = "Cluster Sampling Experimental Design",
             y = "Units per Cluster",
             x = "Clusters") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
}

shinyApp(ui = ui, server = server)
