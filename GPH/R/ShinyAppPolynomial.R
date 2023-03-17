# Run the app
shinyPol <- function () {
  # Load necessary packages
  library(shiny)
  library(ggplot2)
  library(reshape2)

  # Define the user interface
  ui <- fluidPage(
    titlePanel("Polynomial Covariance Progression"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "degree",
                    label = "Degree of Polynomial",
                    min = 1, max = 10, value = 1),
        numericInput(inputId = "sigma",
                     label = "Sigma",
                     value = 1),
        actionButton("plotBtn", "Plot")
      ),
      mainPanel(
        plotOutput(outputId = "plot")
      )
    )
  )

  # Define the server logic
  server <- function(input, output) {

    # Define a function to calculate the polynomial covariance
    poly_cov <- function(x, degree, sigma) {
      outer(x, x, function(x1, x2) {
        sigma^2 * (1 + x1 * x2)^degree
      })
    }

    # Define a reactive expression to plot the polynomial covariance
    plot_poly_cov <- reactive({
      x <- seq(-5, 5, length.out = 100)
      cov_mat <- poly_cov(x, input$degree, input$sigma)
      melted_cov_mat <- melt(cov_mat)
      ggplot(data = melted_cov_mat, aes(x = Var1, y = Var2, fill = value)) +
        geom_raster() +
        scale_fill_gradient(low = "white", high = "steelblue") +
        labs(x = "X", y = "Y", fill = "Covariance") +
        theme_bw()
    })

    # Define an event handler to plot the polynomial covariance
    observeEvent(input$plotBtn, {
      output$plot <- renderPlot({
        plot_poly_cov()
      })
    })
  }


  shinyApp(ui = ui, server = server)
}



