shinyConst <- function () {
  library(shiny)
  library(ggplot2)

  ui <- fluidPage(
    titlePanel("Constant Covariance Progression"),
    sidebarLayout(
      sidebarPanel(
        numericInput("n", label = "Number of observations", value = 100),
        sliderInput("rho", label = "Correlation coefficient", min = -1, max = 1, value = 0.5, step = 0.1),
        actionButton("plot", "Plot")
      ),
      mainPanel(
        plotOutput("cov_plot")
      )
    )
  )

  server <- function(input, output) {
    observeEvent(input$plot, {
      # Generate data
      x <- rnorm(input$n)
      y <- rnorm(input$n)
      z <- input$rho*x + sqrt(1 - input$rho^2)*y
      data <- data.frame(x, y, z)

      # Calculate covariance matrix for each subset of data
      cov_mat <- sapply(seq_len(input$n), function(i) {
        cov(data[1:i, ])
      })

      # Create plot
      output$cov_plot <- renderPlot({
        ggplot() +
          geom_line(aes(seq_len(input$n), cov_mat[1, ], color = "Covariance")) +
          geom_line(aes(seq_len(input$n), rep(1, input$n), color = "Constant"), linetype = "dashed") +
          labs(x = "Number of observations", y = "Covariance") +
          scale_color_manual(values = c("Covariance" = "blue", "Constant" = "red")) +
          theme_minimal()
      })
    })
  }

  shinyApp(ui, server)
}


