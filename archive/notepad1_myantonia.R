library(gutenbergr)
library(magrittr)
library(cowplot)
library(tidyverse)
library(tidytext)
library(ggchicklet)
library(rcolorUtrecht)
library(dutchmasters)
library(jtools)

# (000) make a custom ggplot2 theme
my_theme <- function() {
  theme_apa(legend.pos = "none") +
    theme(panel.background = element_blank(),
          plot.background = element_rect(fill = "antiquewhite"),
          panel.border = element_blank(),                     # facet border
          strip.background = element_blank(),                 # facet title background
          plot.margin = unit(c(.5, .5, .5, .5), "cm")) 
}

# (001) download the novel
gutenberg_works(author=="Cather, Willa")
df <- gutenberg_download(242, mirror = "http://aleph.gutenberg.org")
df

# (002) prepare novel
df <- df %>% 
  slice(-(1:75)) %>% 
  select(-gutenberg_id) %>% 
  mutate(line_number=row_number(),
         book_name=cumsum(str_detect(text, regex("^BOOK [\\divxlc]", ignore_case = TRUE))),
         book_number=cumsum(str_detect(text, regex("^BOOK [\\divxlc]", ignore_case = TRUE))))

df$book_name[which(df$book_name=="0")]="Introduction"
df$book_name[which(df$book_name=="1")]="Book 1: The Shimerdas"
df$book_name[which(df$book_name=="2")]="Book 2: The Hired Girls"
df$book_name[which(df$book_name=="3")]="Book 3: Lena Lingard"
df$book_name[which(df$book_name=="4")]="Book 4: The Pioneer Woman's Story"
df$book_name[which(df$book_name=="5")]="Book 5: Cuzak's Boys"

view(df)


# (003) count number of sentences (per book in the novel)
# (004) count number of words (per book in the novel)
df %>% 
  group_by(book_name) %>% 
  unnest_tokens(output=word, input=text, token="words") %>% 
  summarise(total_word_count=n()) %>% 
  ggplot(mapping = aes(x=reorder(book_name, -total_word_count),
                       y=total_word_count)) +
  geom_chicklet(width = 0.75, 
                radius = grid::unit(5, "mm"),
                fill = "#003300",
                color = "#003300") +
  geom_text(aes(label=total_word_count), 
            vjust=2, 
            color="#9ecb91", 
            size=4.5) +
  my_theme() +
  theme(axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(vjust = 300))


# df %>% 
#   group_by(book_name) %>% 
#   unnest_tokens(word, text) %>% 
#   inner_join(get_sentiments("bing")) %>%
#   add_count(sentiment) %>% 
#   summarize(average_sentiment=mean(n))

# (005) perform a sentiment analysis
## bing lexicon
sentiment <- df %>% 
  group_by(book_name, book_number, line_number) %>% 
  unnest_tokens(output=word, input=text, token="words") %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing"))

sentiment %>% 
  group_by(book_number, book_name) %>% 
  count(sentiment) %>% 
  ggplot(mapping = aes(x=sentiment,
                       y=n)) +
  geom_chicklet(radius = grid::unit(5, "mm"),
                fill = "#003300",
                color = "#003300") +
  facet_wrap(~book_number) +
  my_theme()

## nrc lexicon
df %>% 
  group_by(book_name, book_number) %>% 
  unnest_tokens(output=word, input=text, token="words") %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(sentiment) %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_chicklet() +
  facet_wrap(~book_name) +
  scale_fill_rcolorUtrecht(palette = "microscope") +
  # scale_fill_viridis_d(option = "B") +
  my_theme() + 
  # geom_text(aes(label=n), hjust=-0.5, size=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

## loughran lexicon
df %>% 
  group_by(book_name, book_number) %>% 
  unnest_tokens(output=word, input=text, token="words") %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("loughran")) %>% 
  count(sentiment) %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_chicklet() +
  facet_wrap(~book_name) +
  scale_fill_rcolorUtrecht(palette = "microscope") +
  # scale_fill_viridis_d(option = "B") +
  my_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


## rank books by ratio negative words
sentiment %>% 
  group_by(book_name, book_number) %>% 
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(ratio = negative/(negative+positive)) %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder(book_name, ratio), y = ratio)) + 
  geom_chicklet(radius = grid::unit(6, "mm"),
                fill = "#003300") +
  my_theme() +
  labs(title = "Books ranked by ratio of negative sentiment words",
       y     = "ratio negative words",
       x     = "") +
  coord_flip()


# (006) sentiment over time of the novel
sentiments2 <- df %>% 
  # filter(book_number=="1") %>% 
  group_by(book_name, book_number, line_number) %>% 
  # ungroup() %>% 
  unnest_tokens(word, text) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(index = line_number %/% 100, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

sentiments2 %>% 
  ggplot(aes(index, sentiment, fill = sentiment)) +
  geom_vline(data = book_number[-1,],
             aes(xintercept = index - 0.5),linetype = "dashed") +
  geom_col(show.legend = FALSE,position = "dodge") +
  scale_fill_viridis_c(option = "B") + 
  labs(title = "Romeo and Juliet") +
  annotate("text", x = acts$index + 2, y = 22, label = paste0("act ", acts$act), size = 5) +
  my_theme()


# lexical diversity
df %>% 
  group_by(book_name, line_number) %>% 
  unnest_tokens(word, text) %>% 
  # anti_join(stop_words) %>% 
  count(book_name, word, sort = T) %>% 
  top_n(n=6, n) %>% 
  ggplot(aes(x = reorder_within(word, n, book_name), y = n, fill = book_name)) +
  geom_chicklet()
  


######################################################

# better sentiment analysis


# (007) determine gender roles in the novel
bigrams <- df %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_seperated <- bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

he_she_words <- bigrams_seperated %>% 
  filter(word1 %in% c("he", "she"))

he_she_counts <- he_she_words %>%
  count(word1, word2) %>%
  spread(word1, n, fill = 0) %>%
  mutate(total = he + she,
         he = (he + 1) / sum(he + 1),
         she = (she + 1) / sum(she + 1),
         log_ratio = log2(she / he),
         abs_ratio = abs(log_ratio)) %>%
  arrange(desc(log_ratio))

he_she_words %>% 
  count(word1, word2) %>% 
  spread(word1, n, fill = 0) %>% 
  mutate(total = he + she,
         he = (he + 1) / sum(he + 1),
         she = (she + 1) / sum(she + 1),
         log_ratio = log2(she/he),
         abs_ratio = abs(log_ratio)) %>% 
  arrange(desc(log_ratio))

pronouns <- c("he", "she")

bigram_counts <- df %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = T) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(word1 %in% pronouns) %>% 
  count(word1, word2, wt = n, sort = T) %>% 
  rename(total = n)

bigram_counts %>% group_by(word2) %>% 
  filter(sum(total) > 10) %>% 
  ungroup() %>% 
  spread(word1, total, fill = 0) %>% 
  # mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>% 
  mutate(logratio = log2(she / he)) %>% 
  arrange(desc(logratio)) %>% 
  arrange(abs(logratio)) %>% 
  mutate(abslogratio = abs(logratio)) %>% 
  group_by(logratio < 0) %>% 
  top_n(15, abslogratio) %>% 
  ungroup() %>% 
  mutate(word = reorder(word2, logratio)) %>% 
  ggplot(aes(word, logratio, color = logratio <0)) +
  geom_segment(aes(x = word, xend = word,
                   y = 0, yend = logratio),
               size = 1.1, alpha = 0.6) +
  geom_point(size = 3.5) +
  coord_flip() + my_theme() + 
  scale_color_discrete(name = "", labels=c("More 'she'", "More 'he'")) +
  scale_y_continuous(breaks = seq(-3, 3),
                     labels = c("0.125x", "0.25x", "0.5x", "Same", "2x", "4x", "8x")) +
  labs(title = "Establishing gender roles in the novel My Antonia by correlating gender associated words",
       subtitle = "While women have and ask, men like and think",
       y = "Relative use of words between genders",
       x = "")


he_she_counts %>%
  filter(!word2 %in% c("himself", "herself", "she"),
         total>= 100) %>%
  ggplot(aes(total, log_ratio)) +
  geom_point() +
  scale_x_log10(breaks = c(100, 1000, 10000, 1e5),
                labels = comma_format()) +
  geom_text(aes(label = word2), vjust = 1, hjust = 1,
            check_overlap = true) +
  scale_y_continuous(breaks = seq(-2, 2),
                     labels = c('4x "he"', '2x "he"', "same", '2x "she"', '4x "she"')) +
  labs(x = 'total uses after "he" or "she" (note log scale)',
       y = 'relative uses after "she" to after "he"',
       title = "gendered verbs: comparing frequency to pronoun shift",
       subtitle = "only words occurring at least 100 times after he/she. overlapping labels were removed.") +
  expand_limits(x = 75)


# (008) determine the top 15 most occurring words over the whole novel
df %>% 
  # group_by(book_name, book_number) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  filter(!word=="house",
         !word=="head",
         !word=="don’t") %>% 
  count(word, sort = T) %>% 
  head(n = 15) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_chicklet(fill = "darkgreen") +
  coord_flip() +
  geom_text(aes(label=n), 
            hjust=1.2, 
            color="antiquewhite", 
            size=4.5) +
  my_theme() +
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  labs(title = "Top 15 most occurring words in the novel My Antonia by Willa Cather (1918)")

# (009) determine the top 10 words per book in the novel
introduction <- df %>% 
  filter(book_name=="Introduction") %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  filter(!word=="house",
         !word=="head",
         !word=="don’t") %>% 
  count(word, sort = T) %>% 
  head(n = 10) %>% 
  ggplot(aes(x=reorder(word, n), y = n))+ 
  geom_chicklet(fill = "blue") +
  coord_flip() +
  geom_text(aes(label=n), 
            hjust=2.5, 
            color="#f2eadf", 
            size=4.5) +
  my_theme() +
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Introduction")

book1 <- df %>% 
  filter(book_name=="The Shimerdas") %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  filter(!word=="house",
         !word=="head",
         !word=="don’t") %>% 
  count(word, sort = T) %>% 
  head(n = 10) %>% 
  ggplot(aes(x=reorder(word, n), y = n))+ 
  geom_chicklet(fill = "blue") +
  coord_flip() +
  geom_text(aes(label=n), 
            hjust=2.5, 
            color="#f2eadf", 
            size=4.5) +
  my_theme() +
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "The Shimerdas")

book2 <- df %>% 
  filter(book_name=="The Hired Girls") %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  filter(!word=="house",
         !word=="head",
         !word=="don’t") %>% 
  count(word, sort = T) %>% 
  head(n = 10) %>% 
  ggplot(aes(x=reorder(word, n), y = n))+ 
  geom_chicklet(fill = "blue") +
  coord_flip() +
  geom_text(aes(label=n), 
            hjust=2.5, 
            color="#f2eadf", 
            size=4.5) +
  my_theme() +
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "The Hired Girls")

book3 <- df %>% 
  filter(book_name=="Lena Lingard") %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  filter(!word=="house",
         !word=="head",
         !word=="don’t") %>% 
  count(word, sort = T) %>% 
  head(n = 10) %>% 
  ggplot(aes(x=reorder(word, n), y = n))+ 
  geom_chicklet(fill = "blue") +
  coord_flip() +
  geom_text(aes(label=n), 
            hjust=2.5, 
            color="#f2eadf", 
            size=4.5) +
  my_theme() +
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Lena Lingard")

book4 <- df %>% 
  filter(book_name=="The Pioneer Woman's Story") %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  filter(!word=="house",
         !word=="head",
         !word=="don’t",
         !word=="didn’t",
         !word=="i’m",
         !word=="she’d") %>% 
  count(word, sort = T) %>% 
  head(n = 10) %>% 
  ggplot(aes(x=reorder(word, n), y = n))+ 
  geom_chicklet(fill = "blue") +
  coord_flip() +
  geom_text(aes(label=n), 
            hjust=2.5, 
            color="#f2eadf", 
            size=4.5) +
  my_theme() +
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "The Pioneer Woman's Story")

book5 <- df %>% 
  filter(book_name=="Cuzak's Boys") %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  filter(!word=="house",
         !word=="head",
         !word=="don’t") %>% 
  count(word, sort = T) %>% 
  head(n = 10) %>% 
  ggplot(aes(x=reorder(word, n), y = n))+ 
  geom_chicklet(fill ="blue") +
  coord_flip() +
  geom_text(aes(label=n), 
            hjust=2.5, 
            color="#f2eadf", 
            size=4.5) +
  my_theme() +
  theme(axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Cuzak's Boys")

plot_grid(introduction,
          book1,
          book2,
          book3,
          book4,
          book5,
          # labels = c("", "Book 1: The Shimerdas", "Book 2:", "Book 3:", "Book 4:", "Book 5:"),
          nrow = 3)


####################################################

# Most common positive and negative words

df %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!word=="like",
         !word=="well") %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  # ungroup() %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_chicklet() +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  my_theme() +
  scale_fill_brewer(palette = "Greens")


# Method 2: Most common negative and positive words
bing <- get_sentiments("bing")
bing_word_counts <- df %>% 
  unnest_tokens(word, text) %>% 
  inner_join(bing) %>% 
  count(word, sentiment, sort = T) %>% 
  ungroup()

bing_word_counts %>%
  top_n(40) %>% 
  mutate(n = ifelse(sentiment == 'negative', -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_chicklet() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab('Contribution to sentiment') + ggtitle('Most common positive and negative words') +
  my_theme() +
  scale_fill_dutchmasters(palette = "milkmaid")


# Find the most common punctuations per book
punctuation <- df %>% 
  unnest_tokens(token, text, strip_punct = F) %>% 
  count(book_name, token, sort = T) %>% 
  filter(token %in% c("!", "?", "."))

punctuation %>% 
  mutate(token = reorder(token, n)) %>% 
  ggplot(aes(token, n, fill = book_name)) +
  geom_chicklet() +
  facet_wrap(~book_name, scales = "free_x") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_dutchmasters(palette = "milkmaid") +
  my_theme()


# (003) count number of sentences (per book in the novel)
total_sentences <- punctuation %>% 
  group_by(book_name) %>% 
  summarize(total_sentences = sum(n))


total_sentences %>% 
  ggplot(aes(book_name, total_sentences, fill = book_name)) +
  geom_chicklet() + 
  coord_flip() +
  scale_fill_dutchmasters(palette = "milkmaid")

# (004) count number of words (per book in the novel)
total_words <- df %>% 
  group_by(book_name) %>% 
  unnest_tokens(word, text) %>% 
  count(book_name, word, sort = T) %>% 
  # add_count(word) %>% 
  summarize(total_words = sum(n))


total_words %>% 
  ggplot(aes(reorder(book_name, -total_words), total_words)) +
  geom_chicklet() +
  coord_flip() +
  my_theme()


# bigrams
df_bigrams <- df %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# df_bigrams %>% count(bigram, sort = T)

df_bigrams_seperated <- df_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

df_bigrams_filtered <- df_bigrams_seperated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


bigrams_united <- df_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# bigram_tf_idf <- bigrams_united %>%
#   count(book_name, bigram) %>%
#   bind_tf_idf(bigram, book_name, n) %>%
#   arrange(desc(tf_idf))


bigram_graph <- bigram_counts %>%
  top_n(100) %>% 
  graph_from_data_frame()

set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


# correlating names with words
df_section_words <- df %>% 
  mutate(section = row_number() %/% 10) %>% 
  filter(section > 0) %>% 
  unnest_tokens(word, text) %>% 
  filter(!word %in% stop_words$word)

library(widyr)
word_pairs <- df_section_words %>%
  pairwise_count(word, section, sort = TRUE)


word_cors <- df_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)


word_cors %>%
  filter(item1 == "ántonia")


word_cors %>%
  filter(item1 %in% c("ántonia", "lena", "married", "girls")) %>%
  filter(!item1=="marry", 
         !item2=="marry", 
         !item2=="shimerdas",
         !item1=="she’s",
         !item2=="she’s",
         !item1=="didn’t",
         !item2=="didn’t") %>% 
  group_by(item1) %>%
  slice_max(correlation, n = 10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_chicklet() +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


books <- df %>% 
  unnest_tokens(word, text) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(book_name, index = line_number %/% 100, sentiment) %>% 
  mutate(new_book = book_name != shift(book_name, 1)) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  filter(new_book == T, book_name > 0) %>% 
  select(index, new_book)
books

sentiments3 <- df %>% 
  unnest_tokens(word, text) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(index = line_number %/% 100, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
sentiments3

sentiments3 %>% 
  ggplot(aes(index, sentiment, fill = sentiment)) +
  geom_vline(data = books[0,],
             aes(xintercept = index - 0.5), linetype = "dashed") +
  geom_col(show.legend = F, position = "dodge") +
  scale_fill_viridis_c(option = "B") +
  annotate("text", x = books$index + 2, y = 22, label = paste0("new_book ", books$new_book), size = 3) +
  my_theme()

###################################################

# Combine all aquired info into a single tibble

df_summary <- left_join(total_sentences, total_words, by = "book_name")
View(df_summary)
