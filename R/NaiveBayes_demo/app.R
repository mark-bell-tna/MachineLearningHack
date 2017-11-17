#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidytext)
library(gutenbergr)
library(dplyr)
library(stringr)
library(tidyr)
library(e1071)
library(ggplot2)

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

#books <- gutenberg_works(title %in% titles) %>%
#  gutenberg_download(meta_fields = "title")

#saveRDS(books,"gutenberg_books.rds")
books <- readRDS("RDSfiles//gutenberg_books.rds")


# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

chapters <- unique(by_chapter$document)

by_chapter$book <- substr(by_chapter$document,1,3)

by_chapter[by_chapter$book == "The","book"] <- "1"
by_chapter[by_chapter$book == "Twe","book"] <- "2"
by_chapter[by_chapter$book == "Pri","book"] <- "3"
by_chapter[by_chapter$book == "Gre","book"] <- "4"
by_chapter$book <- as.factor(as.numeric(by_chapter$book))

by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

by_chapter_word <- by_chapter_word[!(grepl("_|,", by_chapter_word$word)),]

word_counts <- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>%
  count(book, document, word, sort = TRUE) %>%
  ungroup()

full_word_counts <- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  ungroup()

uncommon_words <- full_word_counts[full_word_counts$n <= 5,]

common_words <- word_counts %>%
  anti_join(uncommon_words, by = "word")

book_words <- common_words %>%
  bind_tf_idf(word, document, n)

chapters_counts_dtm <- book_words %>%
  cast_dtm(document, word, n)

chapters_tfidf_dtm <- book_words %>%
  cast_dtm(document, word, tf_idf)

dtm_to_df <- function(dtm) {
  df <- as.data.frame(as.matrix(dtm))
  df$document <- row.names(df)
  df$book <- substr(df$document,1,3)
  
  df[df$book == "The","book"] <- "1"
  df[df$book == "Twe","book"] <- "2"
  df[df$book == "Pri","book"] <- "3"
  df[df$book == "Gre","book"] <- "4"
  df$book <- as.factor(as.numeric(df$book))
  df <- df[ , names(df) != "document"]
  df
}

chapters_counts_df <- dtm_to_df(chapters_counts_dtm)
chapters_tfidf_df <- dtm_to_df(chapters_tfidf_dtm)
 

set.seed(123)
train_ind <- sample(seq_len(length(chapters)),
                    size = floor(0.25 * length(chapters)))


training_data_counts <- chapters_counts_df[train_ind,]
validation_data_counts <- chapters_counts_df[-train_ind,]

training_data_tfidf <- chapters_tfidf_df[train_ind,]
validation_data_tfidf <- chapters_tfidf_df[-train_ind,]


#model_counts <- naiveBayes(book ~ ., data = training_data_counts)
#saveRDS(model_counts, 'model_counts.rds')
model_counts <- readRDS('RDSfiles//model_counts.rds')
#model_tfidf <- naiveBayes(book ~ ., data = training_data_tfidf)
#saveRDS(model_tfidf, 'model_tfidf.rds')
model_tfidf <- readRDS('RDSfiles//model_tfidf.rds')

#pred_counts <- predict(model_counts, validation_data_counts)
#saveRDS(pred_counts, 'pred_counts.rds')
pred_counts <- readRDS('RDSfiles//pred_counts.rds')
#pred_tfidf <- predict(model_tfidf, validation_data_tfidf)
#saveRDS(pred_tfidf, 'pred_tfidf.rds')
pred_tfidf <- readRDS('RDSfiles//pred_tfidf.rds')

## Naive Bayes - Confusion Matrix

confusion_counts <- table(pred=pred_counts,true=validation_data_counts$book)
confusion_tfidf <- table(pred=pred_tfidf,true=validation_data_counts$book)




library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Classic Books"),
   
   # Sidebar with a slider input for number of bins 
   tabsetPanel(
      tabPanel("Chapters",
         tableOutput("by_chapter")
      ),
      tabPanel("Words",
         tableOutput("words")
      ),
      tabPanel("Matrix",
         tableOutput("matrix")
      ),
      tabPanel("TFIDF",
         plotOutput("tfidf")
      ),
      tabPanel("Confusion by count",
         plotOutput("confusion_counts")
      ),
      tabPanel("Confusion by tfidf",
               plotOutput("confusion_tfidf"))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$by_chapter <- renderTable({
      # generate bins based on input$bins from ui.R
     data.frame(by_chapter[1:10,])
   })
   
   output$words <- renderTable(({
     data.frame(book_words[1:10,])
   }))
   
   output$matrix <- renderTable({
     head(data.frame(training_data[, c("elizabeth", "darcy", "martians", "nemo", "pip")]),10)
   }, rownames = TRUE, digits = 4)
   
   output$tfidf <- renderPlot({
     plot_tfidf <- book_words %>%
       arrange(desc(tf_idf)) %>%
       mutate(word = factor(word, levels = rev(unique(word))))
     
     plot_tfidf %>% 
       group_by(book) %>% 
       top_n(15) %>% 
       ungroup %>%
       ggplot(aes(word, tf_idf, fill = book)) +
       geom_col(show.legend = FALSE) +
       labs(x = NULL, y = "tf-idf") +
       facet_wrap(~book, ncol = 2, scales = "free") +
       coord_flip()
   })
   
   output$confusion_counts <- renderPlot({
     
     confusion_plot_counts <- ggplot(data.frame(confusion_counts))
     confusion_plot_counts + geom_tile(aes(x=pred, y=true, fill=Freq)) + 
       scale_x_discrete(name="Actual Class") +
       scale_y_discrete(name="Predicted Class") +
       scale_fill_gradient(breaks=seq(from=0, to=50, by=5),
                           high = "red", low = "yellow") +
       labs(fill="Normalized\nFrequency")
     
   })
   
   output$confusion_tfidf <- renderPlot({
     
     confusion_plot_tfidf <- ggplot(data.frame(confusion_tfidf))
     confusion_plot_tfidf + geom_tile(aes(x=pred, y=true, fill=Freq)) + 
       scale_x_discrete(name="Actual Class") +
       scale_y_discrete(name="Predicted Class") +
       scale_fill_gradient(breaks=seq(from=0, to=50, by=5),
                           high = "red", low = "yellow") +
       labs(fill="Normalized\nFrequency")
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

