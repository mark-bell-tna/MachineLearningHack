#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Overfitting Demo"),
  
  # Sidebar with a slider input for number of bins 
  fluidPage(
    inputPanel(
      #actionButton("go", "Go!")
      sliderInput("neighbours", "Degree", 1, 20, 1, step = 1, 
                  animate=animationOptions(interval=1000, loop = F,
                                           playButton = T, pauseButton = T))
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      textOutput("mse")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  mse <- reactiveValues(train = 0, test = 0)
  #First, always remember use to set.seed(n) when generating pseudo random numbers. By doing this, the random number generator generates always the same numbers.
  set.seed(20)
  #Predictor (q). Use seq for generating equally spaced sequences fast
  q <- seq(from=0, to=20, by=1.0)
  #Value to predict (y):
  y <- 500 + 0.4 * (q-10)^3 + (q-5)^2
  #Some noise is generated and added to the real signal (y):
  noise <- rnorm(length(q), mean=10, sd=100)
  noisy.y <- y + noise
  
  set.seed(109)
  t <- seq(from=0, to=20, by=1.0)
  #Value to predict (y):
  yp <- 500 + 0.4 * (t-10)^3 + (q-5)^2
  #Some noise is generated and added to the real signal (y):
  noise_yp <- rnorm(length(t), mean=10, sd=100)
  noisy.yp <- yp + noise_yp
  
  
  df <- data.frame(x = q, y = noisy.y)
  df_t <- data.frame(x = t, y = noisy.yp)
  
  output$mse <- renderText({
    print(paste(c(mse$train, "\n", mse$test)))
  })
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    nn <- input$neighbours
    model <- lm(y ~ poly(x,nn), df)
    pred_train <- predict(model, df)
    pred_test <- predict(model, df_t)
    mse$train <- sprintf("Train MSE: %8.2f",sum((pred_train - df$y)^2))
    mse$test <- sprintf("Test MSE: %8.2f",sum((pred_test - df_t$y)^2))
    ggplot(df, aes(x = x, y = y)) + geom_point() +
      geom_smooth(method='lm',formula=y~poly(x,nn), se = FALSE, colour = "blue")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
