
# Run the app
shinySquaredExp <- function () {
  # Load required packages
  library(shiny)
  library(ggplot2)

  # Define the UI
  ui <- fluidPage(

    # App title
    titlePanel("Squared Exponential Covariance Progression"),

    # Sidebar with user inputs
    sidebarLayout(
      sidebarPanel(
        # Input for the range of x values
        sliderInput("x_range",
                    "Select x range:",
                    min = 0, max = 10, value = c(0, 10),
                    step = 0.1),
        # Input for the number of points
        sliderInput("n_points",
                    "Select number of points:",
                    min = 10, max = 100, value = 50,
                    step = 1)
      ),
      # Main panel for the plot
      mainPanel(
        plotOutput("cov_plot")
      )
    )
  )

  # Define the server
  server <- function(input, output) {

    # Define the function for computing the squared exponential covariance
    cov_func <- function(x1, x2, l=1, sigma=1) {
      cov_matrix <- matrix(0, nrow=length(x1), ncol=length(x2))
      for (i in seq_along(x1)) {
        for (j in seq_along(x2)) {
          cov_matrix[i,j] <- sigma^2 * exp(-(x1[i]-x2[j])^2 / (2*l^2))
        }
      }
      return(cov_matrix)
    }

    # Define the reactive expression for the plot
    cov_plot <- reactive({
      # Generate the x values
      x_values <- seq(input$x_range[1], input$x_range[2], length.out = input$n_points)
      # Compute the covariance matrix
      cov_matrix <- cov_func(x_values, x_values)
      # Create a data frame for the plot
      plot_data <- data.frame(x = rep(x_values, each = length(x_values)),
                              y = rep(x_values, times = length(x_values)),
                              z = as.vector(cov_matrix))
      # Create the plot
      ggplot(plot_data, aes(x = x, y = y, fill = z)) +
        geom_raster() +
        scale_fill_gradient(low = "white", high = "blue") +
        theme_minimal() +
        labs(x = "x", y = "x", fill = "Covariance")
    })

    # Render the plot
    output$cov_plot <- renderPlot({
      cov_plot()
    })
  }


  shinyApp(ui = ui, server = server)
}
