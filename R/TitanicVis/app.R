#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rpart.plot)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

data(ptitanic)
#colnames(ptitanic)[which(names(ptitanic) == "sex")] <- "gender"

titanic_choices <- c("Survived" = "survived", "Ticket Class" = "pclass", "Gender" = "sex",
                     "Age" = "age", "#Siblings/Spouses" = "sibsp", "#Parents/Children" = "parch")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  inputPanel(
    selectInput("column_list", label = "Columns:",
                choices = titanic_choices,
                selectize = TRUE, multiple = TRUE)
  )
  ,
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     columns <- input$column_list
     gg_dataset <- ptitanic
     if (length(columns) == 0) {
       ggplot(gg_dataset) +
         geom_histogram(aes(x = survived, fill = survived), stat = "count", position = "stack") +
         geom_text(aes(x = survived, y = ((..count..)/sum(..count..)),
                       label = scales::percent((..count..)/sum(..count..))), colour = "yellow", stat = "count",                 vjust = -10.2) +
         scale_fill_manual(values = c("Red", "Blue")) +
         labs(title = "Survival rates", y = "People", x = "")
     } else if (length(columns) == 1) {
       column1 <- columns[1]
       out_plot <- ggplot(gg_dataset, aes_string(x = column1))
       if (is.factor(gg_dataset[,column1])) {
         out_plot <- out_plot + geom_bar(aes(y = (..count..)/sum(..count..), fill = survived))
         out_plot <- out_plot + geom_text(aes(y = ((..count..)/sum(..count..)),
                                              label = scales::percent((..count..)/sum(..count..))),
                                          stat = "count", vjust = -0.25)
       } else {
         out_plot <- out_plot + geom_histogram(aes(fill = survived), bins = 10, position = "dodge")
       }
       out_plot <- out_plot + labs(title = paste0("Survival rates by", simpleCap(column1)),
                                   y = "Percent", x = simpleCap(column1))
       out_plot <- out_plot + scale_fill_manual(values = c("Red", "Blue"))
       out_plot
     }
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

