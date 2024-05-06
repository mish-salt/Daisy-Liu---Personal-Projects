library(tidytext)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(wordcloud)
library(gutenbergr)
library(tidyr)

books <- gutenberg_download(c(3207,7370,6762,1232), meta_fields = "author", mirror = "http://mirrors.xmission.com/gutenberg/")

glimpse(books)

books_words <- books %>%
  unnest_tokens(word, text)%>% #tokenize text & group by authors and count
  count(author, word, sort = TRUE)%>%
  anti_join(stop_words) #remove stopwords

head(books_words)

#look at each author separately

hobbes_words <- books_words%>%
  filter(author == "Hobbes, Thomas")

machiavelli_words <- books_words%>%
  filter(author == "Machiavelli, Niccolò")


aristotle_words <- books_words%>%
  filter(author == "Aristotle")

locke_words <- books_words %>%
  filter(author == "Locke, John")

head(hobbes_words, 10)
head(machiavelli_words, 10)
head(aristotle_words, 10)
head(locke_words, 10)

#append tf-idf

tfidf <- books_words %>%
  bind_tf_idf(word, author,n)%>%
  arrange(desc(tf_idf))
# 
# head(tfidf, 20)

facet_plot <- tfidf %>%
  group_by(author)%>%
  slice_max(tf_idf, n = 15) %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ungroup()%>%
  ggplot(aes(tf_idf, word, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~author, ncol = 2, scales = "free")

facet_plot

books <- gutenberg_download(c(3207,7370,6762,1232), meta_fields = "author",  mirror = "http://mirrors.xmission.com/gutenberg/")

glimpse(books)

books_words <- books %>%
  unnest_tokens(word, text)%>% #tokenize text & group by authors and count
  count(author, word, sort = TRUE)%>%
  anti_join(stop_words) #remove stopwords

head(books_words)

#look at each author separately

# hobbes_words <- books_words%>%
#   filter(author == "Hobbes, Thomas")
# 
# machiavelli_words <- books_words%>%
#   filter(author == "Machiavelli, Niccolò")
#   
# 
# aristotle_words <- books_words%>%
#   filter(author == "Aristotle")
# 
# locke_words <- books_words %>%
#   filter(author == "Locke, John")

# head(hobbes_words, 10)
# head(machiavelli_words, 10)
# head(aristotle_words, 10)
# head(locke_words, 10)

#append tf-idf

tfidf <- books_words %>%
  bind_tf_idf(word, author,n)%>%
  arrange(desc(tf_idf))
# 
# head(tfidf, 20)

facet_plot <- tfidf %>%
  group_by(author)%>% #see the ranked words for each author separately
  slice_max(tf_idf, n = 15) %>% #take top 15
  mutate(word = reorder(word, tf_idf)) %>% #displays plot in descending order 
  ungroup()%>%
  ggplot(aes(tf_idf, word, fill = author)) + #x = tfidf, y = word, color = author
  geom_col(show.legend = FALSE) + #no legend >:(
  labs(x = "tf-idf", y = NULL) + #labels 
  facet_wrap(~author, ncol = 2, scales = "free_y") #facet plot based on authors, with two display rows

facet_plot

#_______________________ N-GRAMS_________________________________
#Unnest text into bigrams & conduct a tf-idf analysis

books_bigram <- books %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

head(books_bigram, 15)

#separate bigrams into different columns, remove NA and stop words, count

sep_bigram <- books_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>% #separate bigram into columns (word1 and word2), using a space as indication of where to separate
  filter(!word1 %in% stop_words$word) %>% #keep words in word1 that are not in stopwords list
  filter(!word2 %in% stop_words$word)%>%
  drop_na()%>% #eliminate NA values
  count(author, word1, word2, sort = TRUE) #group by author, word1 and word2, count and sort


head(sep_bigram,15)

#unite the separated columns, apply tf-idf

bigram_tfidf<- sep_bigram %>%
  unite(bigram, word1, word2, sep = " ") %>%
  bind_tf_idf(bigram, author, n)%>%
  arrange(desc(tf_idf))


head(bigram_tfidf)

#bigram tf-idf plot 

bigram_tfidf_plot <- bigram_tfidf%>%
  group_by(author)%>%
  slice_max(tf_idf, n = 10) %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ungroup()%>%
  ggplot(aes(tf_idf, bigram, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~author, ncol = 2, scales = "free_y")

bigram_tfidf_plot

#GRAPHING BIGRAMS
#try seeing the bigram relationship in Hobbe's work

hobbes_sep_bigram <- sep_bigram %>%
  filter(author == "Hobbes, Thomas")%>%
  select(!author)

head(hobbes_sep_bigram)

#preparing for graphing
set.seed(123)
library(igraph)
library(ggraph)

hobbes_graph <- hobbes_sep_bigram %>%
  filter(n > 15)%>%
  graph_from_data_frame()

#visualize graph

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))#arrow setting

hobbes_vis <- ggraph(hobbes_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

hobbes_vis  
