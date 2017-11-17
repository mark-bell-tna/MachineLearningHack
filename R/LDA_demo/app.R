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
library(topicmodels)
library(shiny)

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


book_words <- word_counts %>%
  bind_tf_idf(word, document, n)


chapters_dtm <- book_words %>%
  cast_dtm(document, word, n)

#chapters_lda_4 <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
#saveRDS(chapters_lda_4, 'R/LDA_demo/chapters_lda_4.rds')
chapters_lda_4 <- readRDS('RDSfiles//chapters_lda_4.rds')
#chapters_lda_9 <- LDA(chapters_dtm, k = 9, control = list(seed = 1234))
#saveRDS(chapters_lda_9, 'R/LDA_demo/chapters_lda_9.rds')
chapters_lda_9 <- readRDS('RDSfiles//chapters_lda_9.rds')


chapter_topics_4 <- tidy(chapters_lda_4, matrix = "beta")
chapter_topics_9 <- tidy(chapters_lda_9, matrix = "beta")

top_terms_4 <- chapter_topics_4 %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_9 <- chapter_topics_9 %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

chapters_gamma <- tidy(chapters_lda_4, matrix = "gamma")
#chapters_gamma

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Classic Books"),
   
   # Sidebar with a slider input for number of bins 
   tabsetPanel(
      tabPanel("Matrix",
         tableOutput("matrix")
      ),
      tabPanel("4 Topics",
         plotOutput("topterms_4")
      ),
      tabPanel("9 Topics",
               plotOutput("topterms_9")
      ),
      tabPanel("Classifications",
               plotOutput("classifications"))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

   output$matrix <- renderTable({
     set.seed(123)
     train_ind <- sample(seq_len(length(chapters)),
                         size = floor(0.25 * length(chapters)))
     head(data.frame(as.matrix(chapters_dtm[train_ind, 
                                            c("pip","martians","nautilus","darcy")])),10)
   }, rownames = TRUE)
   
   output$topterms_4 <- renderPlot({
     top_terms_4 %>%
       mutate(term = reorder(term, beta)) %>%
       ggplot(aes(term, beta, fill = factor(topic))) +
       geom_col(show.legend = FALSE) +
       facet_wrap(~ topic, scales = "free") +
       coord_flip()
   })

   output$topterms_9 <- renderPlot({
     top_terms_9 %>%
       mutate(term = reorder(term, beta)) %>%
       ggplot(aes(term, beta, fill = factor(topic))) +
       geom_col(show.legend = FALSE) +
       facet_wrap(~ topic, scales = "free") +
       coord_flip()
   })

   output$classifications <- renderPlot({
     chapters_gamma %>%
       mutate(title = reorder(title, gamma * topic)) %>%
       ggplot(aes(factor(topic), gamma)) +
       geom_boxplot() +
       facet_wrap(~ title)
   })   

}

# Run the application 
shinyApp(ui = ui, server = server)

