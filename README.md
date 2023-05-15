# bayesian
#install.packages("shiny")
#install.packages("dplyr")
#install.packages("ggplot2")

library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Bayesian Probability Calculator"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("prior_a", "Prior a", min = 1, max = 100, value = 50),
      sliderInput("prior_b", "Prior b", min = 1, max = 100, value = 50),
      sliderInput("observed_successes", "Observed Successes", min = 0, max = 100, value = 50),
      sliderInput("observed_trials", "Observed Trials", min = 1, max = 100, value = 100)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Posterior Probability Plot",
                 plotOutput("posterior_plot")),
        tabPanel("Posterior Probability Table",
                 tableOutput("posterior_table"))
      )
    )
  )
)

server <- function(input, output) {
  
  posterior_df <- reactive({
    a <- input$prior_a + input$observed_successes
    b <- input$prior_b + input$observed_trials - input$observed_successes
    x <- seq(0, 1, length.out = 100)
    y <- dbeta(x, a, b)
    data.frame(x, y)
  })
  
  posterior_a <- reactive({
    input$prior_a + input$observed_successes
  })
  
  output$posterior_plot <- renderPlot({
    plot(posterior_df()$x, posterior_df()$y, type = "l",
         xlab = "Probability of success",
         ylab = "Posterior probability",
         main = "Posterior Probability Distribution")
    abline(v = input$observed_successes/input$observed_trials, col = "red", lty = 2)
  })
  
  output$posterior_table <- renderTable({
    data.frame(Posterior_Probability = round(posterior_df()$y, 3),
               Probability_of_Success = round(posterior_df()$x, 3))
  })
}

shinyApp(ui = ui, server = server)

