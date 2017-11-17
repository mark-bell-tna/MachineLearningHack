#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ElemStatLearn)
library(class)

ui <- fluidPage(
  
  fluidPage(
    sidebarLayout(
    sidebarPanel(
      #actionButton("go", "Go!")
      sliderInput("neighbours", "Neighbours", 1, 15, 1, step = 1, 
                  animate=animationOptions(interval=2000, loop = T,
                                           playButton = T, pauseButton = T))
      
    , width = 3),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", height = 400, width = 500)
    , width = 9)
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  n <- reactiveValues(neighbours = 1, change = TRUE)
  
  #observeEvent(input$go, {
  #   n$neighbours <- 1
  #  n$change <- !n$change
  #if (n$neighbours == 15) {
  #    n$neighbours <- 1
  # } else {
  #    n$neighbours <- n$neighbours + 1
  # }
  #})
  
  #observeEvent(n$change, {
  #   n$neighbours <- n$neighbours + 1
  #})
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    nn <- input$neighbours
    x <- mixture.example$x
    g <- mixture.example$y
    xnew <- mixture.example$xnew
    mod15 <- knn(x, xnew, g, k=nn, prob=TRUE)
    prob <- attr(mod15, "prob")
    prob <- ifelse(mod15=="1", prob, 1-prob)
    px1 <- mixture.example$px1
    px2 <- mixture.example$px2
    prob15 <- matrix(prob, length(px1), length(px2))
    par(mar=rep(2,4))
    contour(px1, px2, prob15, levels=0.5, labels="", xlab="", ylab="", main=
              paste0(nn,"-nearest neighbour"), axes=FALSE)
    points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
    gd <- expand.grid(x=px1, y=px2)
    points(gd, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
    box()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
