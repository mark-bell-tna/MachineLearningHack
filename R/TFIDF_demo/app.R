#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

dtm <- matrix(0L, nrow = 5, ncol = 7)
dimnames(dtm) = list(c("A","B","C","D","E"),
                     c("cat","fiddled","dog","barked","duck","quacked","go"))
dtm[1,1] <- 1
dtm[1,2] <- 1
dtm[2,3] <- 1
dtm[2,4] <- 1
dtm[3,5] <- 1
dtm[3,6] <- 1
dtm[4,3] <- 1
dtm[4,5] <- 1
dtm[5,5] <- 2
dtm[5,7] <- 1

doc_text <- c("The Cat Fiddled",
              "The dog barked",
              "The duck quacked",
              "The dog and duck",
              "Duck Duck Go!")

dtm_tf <- dtm[,]
dtm_tf[1,] <- dtm_tf[1,] / 2
dtm_tf[2,] <- dtm_tf[2,] / 2
dtm_tf[3,] <- dtm_tf[3,] / 2
dtm_tf[4,] <- dtm_tf[4,] / 2
dtm_tf[5,] <- dtm_tf[5,] / 3

df <- matrix(0L, nrow = 7, ncol = 2)
dimnames(df) <- list(c("cat","fiddled","dog","barked","duck","quacked","go"),
                     c("documents", "idf"))
df[,1] <- c(1,1,2,1,3,1,1)
df[,1] <- 5 / df[,1]
df[,2] <- log(df[,1])
df <- data.frame(df)


doc_words <- tibble(text = doc_text, doc = c("A","B","C","D","E")) %>%
  unnest_tokens(word, text) %>%
  count(doc, word, sort = TRUE) %>%
  ungroup()

total_words <- doc_words %>% 
  group_by(doc) %>% 
  summarize(total = sum(n))

doc_words <- left_join(doc_words, total_words)

doc_words <- doc_words %>%
  bind_tf_idf(word, doc, n)


ui <- fluidPage(
  tabsetPanel(
    tabPanel("Text",
             tableOutput("doc_text"
             )),
    tabPanel("TF",
             tableOutput("term_matrix"),
             tableOutput("tf_matrix")
    ),
    tabPanel("IDF",
             tableOutput("idf")
    ),
    tabPanel("TF-IDF",
             tableOutput("tfidf"))
  )
)

server <- function(input, output, session) {
  output$doc_text <- renderTable({
    data.frame(DocumentText = doc_text)
  })
  
  output$term_matrix <- renderTable({
    data.frame(dtm)
  })
  
  output$tf_matrix <- renderTable({
    data.frame(dtm_tf)
  })
  
  output$idf <- renderTable({
    df
  }, include.rownames=TRUE)
  
  output$tfidf <- renderTable({
    doc_words
  })
  
}

shinyApp(ui, server)
