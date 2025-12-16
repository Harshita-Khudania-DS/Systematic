# ============================================================
# SYSTEMATIC SAMPLING – SHINY APPLICATION
# ============================================================

library(shiny)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  
  titlePanel("Systematic Sampling – Sample Size & Estimation"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Population Settings"),
      
      numericInput("N", "Population Size (N)", value = 60, min = 20),
      numericInput("d", "Allowable Error (d)", value = 2, min = 0.5),
      numericInput("conf", "Confidence Level", value = 0.95, min = 0.8, max = 0.99),
      
      hr(),
      
      actionButton("generate", "Generate Sample", class = "btn-primary")
    ),
    
    mainPanel(
      
      h4("Summary Table"),
      tableOutput("summary"),
      
      hr(),
      
      h4("Subgroup-wise Sample Distribution"),
      tableOutput("subgroup"),
      
      hr(),
      
      h4("Visual Analysis"),
      plotOutput("plots", height = "600px")
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output) {
  
  results <- eventReactive(input$generate, {
    
    set.seed(123)
    
    # -------- Population Generation --------
    population <- data.frame(
      unit_id = 1:input$N,
      subgroup = rep(c("G1", "G2", "G3"), length.out = input$N),
      value = c(rnorm(input$N/3, 50, 8),
                rnorm(input$N/3, 60, 10),
                rnorm(input$N/3, 55, 9))
    )
    
    population <- population[sample(1:nrow(population)), ]
    population$unit_id <- 1:nrow(population)
    
    N <- nrow(population)
    
    # -------- True Parameters --------
    true_mean <- mean(population$value)
    true_variance <- var(population$value)
    
    Z <- qnorm((1 + input$conf) / 2)
    
    # -------- Sample Size --------
    n_sys <- ceiling((Z^2 * true_variance) / input$d^2)
    k <- max(1, floor(N / n_sys))
    
    r <- sample(1:k, 1)
    sample_indices <- seq(r, by = k, length.out = n_sys)
    sample_indices <- sample_indices[sample_indices <= N]
    
    systematic_sample <- population[sample_indices, ]
    
    # -------- Estimation --------
    sample_mean <- mean(systematic_sample$value)
    bias <- sample_mean - true_mean
    
    # -------- Cost & Time --------
    cost_map <- c(G1 = 1, G2 = 1.5, G3 = 2)
    time_map <- c(G1 = 2, G2 = 3, G3 = 4)
    
    systematic_sample$Cost <- cost_map[systematic_sample$subgroup]
    systematic_sample$Time <- time_map[systematic_sample$subgroup]
    
    total_cost <- sum(systematic_sample$Cost)
    total_time <- sum(systematic_sample$Time)
    
    list(
      population = population,
      sample = systematic_sample,
      true_mean = true_mean,
      sample_mean = sample_mean,
      bias = bias,
      n = nrow(systematic_sample),
      cost = total_cost,
      time = total_time,
      k = k
    )
  })
  
  # -------- Summary Table --------
  output$summary <- renderTable({
    req(results())
    
    data.frame(
      Parameter = c("Population Mean", "Sample Mean", "Bias",
                    "Sample Size", "Sampling Interval (k)",
                    "Total Cost", "Total Time"),
      Value = round(c(
        results()$true_mean,
        results()$sample_mean,
        results()$bias,
        results()$n,
        results()$k,
        results()$cost,
        results()$time
      ), 3)
    )
  })
  
  # -------- Subgroup Distribution --------
  output$subgroup <- renderTable({
    req(results())
    as.data.frame(table(results()$sample$subgroup))
  })
  
  # -------- Plots --------
  output$plots <- renderPlot({
    req(results())
    
    par(mfrow = c(2, 2))
    
    # Population
    plot(results()$population$unit_id,
         results()$population$value,
         col = as.numeric(factor(results()$population$subgroup)),
         pch = 19,
         main = "Population Distribution",
         xlab = "Unit ID",
         ylab = "Value")
    
    # Systematic Sample
    plot(results()$population$unit_id,
         results()$population$value,
         type = "l", col = "grey70",
         main = paste("Systematic Sampling (k =", results()$k, ")"),
         xlab = "Unit ID", ylab = "Value")
    
    points(results()$sample$unit_id,
           results()$sample$value,
           col = "red", pch = 19)
    
    # Subgroup Allocation
    barplot(table(results()$sample$subgroup),
            main = "Subgroup-wise Sample Allocation",
            ylab = "Frequency",
            col = "skyblue")
    
    # Mean Comparison
    hist(results()$population$value,
         breaks = 20, col = "lightgrey",
         main = "Population vs Sample Mean",
         xlab = "Value")
    
    abline(v = results()$true_mean, col = "blue", lwd = 2)
    abline(v = results()$sample_mean, col = "red", lwd = 2, lty = 2)
    
    par(mfrow = c(1, 1))
  })
}

# -----------------------------
# RUN APP
# -----------------------------
shinyApp(ui = ui, server = server)
