#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(keras)
#reticulate::use_python("/usr/bin/python")
#install_keras()

mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

dim(x_train) <- c(nrow(x_train), 784)
dim(x_test) <- c(nrow(x_test), 784)
x_train <- x_train / 255
x_test <- x_test / 255

y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

confusion <- NULL

model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(actionButton("run","Run"), width=2),
     mainPanel(
      # Show a plot of the generated distribution
      tabsetPanel(
        tabPanel("Digit",
                 plotOutput("digit")
                 ),
        tabPanel("Matrix",
                 tableOutput("matrix")
                 ),
        tabPanel("Labels",
                 tableOutput("labels")),
        tabPanel("Scaled",
                 tableOutput("scaled")),
        tabPanel("Categorical",
                 tableOutput("categorical")),
        tabPanel("Results",
                 verbatimTextOutput("result"))
      )
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$digit <- renderPlot({
     mat1 <- mnist$train$x[3,1:28,1:28]
     mat1 <- apply(mat1, 2, rev)
     image(1:28, 1:28, t(mat1), asp = 0.75, xlim = c(1,28))
   })
   
   output$matrix <- renderTable({
     t(mnist$train$x[3,10:22,3:15])
   })
   
   output$labels <- renderTable({
     mnist$train$y[1:10]
   })
   
   output$scaled <- renderTable({
     x_train[3,535:545]
   })
   
   output$categorical <- renderTable({
     y_train[1:10,]
   })
   
   output$result <- renderPrint({
     y_test_hat <- predict_classes(model, x_test)
     as.matrix(table(mnist$test$y, y_test_hat))
   })
   
   observeEvent(input$run,
                {
                  history <- model %>% fit(
                    x_train, y_train,
                    epochs = 10, batch_size = 128,
                    validation_split = 0.2
                  )
                })
}

# Run the application 
shinyApp(ui = ui, server = server)

