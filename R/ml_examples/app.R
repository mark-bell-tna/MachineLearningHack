#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

counter <<- 0

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title

   inputPanel(
     actionButton("do", label = "Next")
   ),
   plotOutput("distPlot")
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderImage({
    input$do
    counter <<- counter + 1
    if (counter == 6) {
      counter <<- 1
    }
    filename <- paste0("example",counter,".png")
    print(paste0("Images/",filename))
    print(getwd())
    print(file.exists(paste0("Images/",filename)))
    list(src = paste0("Images/",filename),
         alt = filename)
  }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)




