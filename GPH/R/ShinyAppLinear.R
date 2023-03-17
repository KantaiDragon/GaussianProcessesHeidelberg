
shinyLin <- function () {
  library(shiny)
  library(ggplot2)

  ui <- fluidPage(
    titlePanel("Linear Covariance Progression Plotter"),
    sidebarLayout(
      sidebarPanel(
        numericInput("n", "Number of data points:", value = 20, min = 2, max = 100),
        sliderInput("beta", "Slope of the regression line:", min = -1, max = 1, value = 0.5, step = 0.05),
        sliderInput("sigma", "Standard deviation of noise:", min = 0, max = 1, value = 0.2, step = 0.05),
        actionButton("plot", "Plot")
      ),
      mainPanel(
        plotOutput("covplot")
      )
    )
  )

  server <- function(input, output) {
    data <- eventReactive(input$plot, {
      x <- seq(0, 1, length.out = input$n)
      y <- input$beta * x + rnorm(input$n, sd = input$sigma)
      data.frame(x, y)
    })

    output$covplot <- renderPlot({
      ggplot(data(), aes(x, y)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = "X", y = "Y") +
        theme_bw()
    })
  }

  shinyApp(ui, server)
}

