

library(dplyr)
library(janeaustenr)
library(tidytext)

tibble(text = doc_text, doc = c("A","B","C","D","E")) %>%
  unnest_tokens(word, text)


doc_words <- tibble(text = doc_text, doc = c("A","B","C","D","E")) %>%
  unnest_tokens(word, text) %>%
  count(doc, word, sort = TRUE) %>%
  ungroup()

total_words <- doc_words %>% 
  group_by(doc) %>% 
  summarize(total = sum(n))

doc_words <- left_join(doc_words, total_words)

doc_words

doc_words <- doc_words %>%
  bind_tf_idf(word, doc, n)
doc_words

inspect(AssociatedPress[40:50, 10:15])

data("AssociatedPress")


library(topicmodels)

library(gutenbergr)
library(dplyr)
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")


books

library(stringr)
library(tidyr)
library(tidytext)

# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)
nrow(by_chapter_word)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda
saveRDS(chapters_lda, 'chapters_lda.Rdata')

#chapters_lda <- readRDS('chapters_lda.Rdata')
chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

'chapters_lda.Rdata'

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()





by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

chapters <- unique(by_chapter$document)



by_chapter$book <- substr(by_chapter$document,1,3)
by_chapter$title <- by_chapter$book
unique(by_chapter$book)

by_chapter[by_chapter$book == "The","book"] <- "1"
by_chapter[by_chapter$book == "1","title"] <- "The War of the Worlds"
by_chapter[by_chapter$book == "Twe","book"] <- "2"
by_chapter[by_chapter$book == "2","title"] <- "Twenty Thousand Leagues under the Sea"
by_chapter[by_chapter$book == "Pri","book"] <- "3"
by_chapter[by_chapter$book == "3","title"] <- "Pride and Prejudice"
by_chapter[by_chapter$book == "Gre","book"] <- "4"
by_chapter[by_chapter$book == "4","title"] <- "Great Expectations"
by_chapter$book <- as.numeric(by_chapter$book)

by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

by_chapter_word <- by_chapter_word[!(grepl("_|,", by_chapter_word$word)),]
nrow(by_chapter_word)
# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(book, document, word, sort = TRUE) %>%
  ungroup()

full_word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()

uncommon_words <- full_word_counts[full_word_counts$n <= 5,]

common_words <- word_counts %>%
  anti_join(uncommon_words, by = "word")


book_words <- common_words %>%
  bind_tf_idf(word, document, n)


all_corpus_words <- data.frame(word = unique(common_words$word))

feature_matrix <- matrix(data = 0,nrow = length(chapters),
                             ncol = nrow(all_corpus_words))
class_vector <- vector(length = length(chapters))
for (chap_num in 1:length(chapters)) {
  
  chap_tfidf <- book_words[book_words$document == chapters[chap_num[1]],
                              c("book", "word", "tf_idf")] %>%
    full_join(all_corpus_words, by ="word")
  chap_tfidf[is.na(chap_tfidf$tf_idf), "tf_idf"] <- 0
  features <- as.vector(unlist(chap_tfidf[order(chap_tfidf$word),"tf_idf"]))
  feature_matrix[chap_num,] <- features
  classification <- unique(chap_tfidf$book)
  classification <- classification[!is.na(classification)]
  class_vector[chap_num] <- classification
  print(paste0(chapters[chap_num[1]], " ", classification))
}

rownames(feature_matrix) <- chapters
all_data <- cbind(feature_matrix, class_vector)
colnames(all_data) <- c(as.character(all_corpus_words[order(all_corpus_words),"word"]),
                              "yclassification")

set.seed(123)
train_ind <- sample(seq_len(length(chapters)),
                    size = floor(0.25 * length(chapters)))
training_labels <- class_vector[train_ind]
training_features <- feature_matrix[train_ind,]
training_data <- data.matrix(all_data[train_ind,])
training_chapters <- chapters[train_ind]
validation_labels <- class_vector[-train_ind]
validation_features <- feature_matrix[-train_ind,]
validation_data <- data.matrix(all_data[-train_ind,])

training_data_df <- data.frame(training_data)
training_data_df$yclassification <- as.factor(training_data_df$yclassification)
model <- naiveBayes(yclassification ~ ., data = training_data_df)
validation_data_df <- data.frame(validation_data)
validation_data_df$yclassification <- as.factor(validation_data_df$yclassification)

pred <- predict(model, validation_data_df)
#confusion matrix
confusion <- table(pred=pred,true=validation_data_df$yclassification)
confusion

plot <- ggplot(data.frame(confusion))
plot + geom_tile(aes(x=pred, y=true, fill=Freq)) + scale_x_discrete(name="Actual Class") +
  scale_y_discrete(name="Predicted Class") +
  scale_fill_gradient(breaks=seq(from=-.5, to=4, by=.2)) +
  labs(fill="Normalized\nFrequency")

preds <- predict(model, newdata = validation_data_df)
preds


training_text <- book_words[book_words$document %in% training_chapters,]
validation_text <- book_words[book_words$document %in% validation_labels,]

training_text[order(training_text$document)]

plot_tfidf <- book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_training %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

plot_validation <- validation_text %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_validation %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

library( 'e1071' )
data( iris )
model <- svm( iris$Species~., iris )
res <- predict( model, newdata=iris )


as.matrix(training_text$tf_idf, )
