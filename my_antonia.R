# (00) load packages
if(!require(rcolorUtrecht)) install.packages("rcolorUtrecht")   # provides color palettes
if(!require(ggchicklet)) install.packages("ggchicklet")         # rounded bar plots
if(!require(gutenbergr)) install.packages("gutenbergr")         # the gutenbergr database
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(tidytext)) install.packages("tidytext")             # making text data tidy
if(!require(cowplot)) install.packages("cowplot")               # combining multiple plots
if(!require(jtools)) install.packages("jtools")                 # regression analysis


# (00) make a custom theme
# Source: https://peerchristensen.netlify.app/post/fair-is-foul-and-foul-is-fair-a-tidytext-entiment-analysis-of-shakespeare-s-tragedies/
my_theme <- function() {
  theme_apa(legend.pos="none") +
    theme(panel.background=element_blank(),                 # remove plot grids
          plot.background=element_rect(fill="#f3faff"),     # background color
          panel.border=element_blank(),                     # facet border
          strip.background=element_blank(),                 # facet title background
          plot.margin = unit(c(.5, .5, .5, .5), "cm")) 
}

# (001) mine novel from gutenberg.org using the gutenbergr package
gutenberg_works(author=="Cather, Willa")
df <- gutenberg_download(242, mirror = "http://aleph.gutenberg.org")


# (002) data cleaning and tidying
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


# (003) determine which punctuations are most commonly used
punctuation <- df %>% 
  unnest_tokens(token, text, strip_punct = F) %>% 
  count(book_name, token, sort = T) %>% 
  filter(token %in% c("!", "?", "."))
punctuation

punctuation %>% 
  mutate(token = reorder(token, n)) %>% 
  ggplot(mapping=aes(x=token, y=n, fill=book_name)) +
  geom_chicklet(width = 0.75,
                radius=grid::unit(2,"mm")) +
  facet_wrap(~book_name, scales = "free_x") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0)) +
  my_theme() + 
  theme(axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_text(margin = margin(10,0,0,0)),
        plot.caption=element_text(face="italic", margin = margin(10,0,0,0))) +
  ggtitle("Most common punctuations used per book in novel My Antonia") +
  labs(caption="Data was mined from the gutenberg.org database using the gutenbergr package.",
       y="Number of times punctuation was used") +
  scale_fill_rcolorUtrecht(palette = "hu")


# (004) count number of sentences
total_sentences <- punctuation %>% 
  group_by(book_name) %>% 
  summarize(total_sentences = sum(n))
total_sentences

total_sentences %>% 
  ggplot(mapping=aes(x=reorder(book_name, total_sentences), 
                     y=total_sentences, 
                     fill = book_name)) +
  geom_chicklet(width=0.75,
                radius=grid::unit(2,"mm"),
                fill="#0047ab", color="#0047ab") + 
  my_theme() +
  coord_flip() +
  geom_text(aes(label=total_sentences), vjust=0.5, hjust=1.2, color="#f3faff", size=3) +
  theme(axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        plot.caption=element_text(face="italic"),
        axis.text.x = element_text(hjust=1.5)) +
  ggtitle("Number of sentences per book in the novel\nMy Antonia (1918)") +
  labs(x="", y="", 
       subtitle="The longer the book, the higher the sentence count.",
       caption = "Data was mined from the gutenberg.org database using the gutenbergr package.")


# (005) count number of words
total_words<-df %>%
  group_by(book_name) %>%
  unnest_tokens(output=word, input=text, token="words") %>%
  summarize(total_word_count=n())
total_words

total_words %>%
  ggplot(mapping=aes(x=reorder(book_name, -total_word_count), 
                     y=total_word_count)) +
  geom_chicklet(width=0.7,
                radius=grid::unit(2,"mm"),
                fill="#0047ab", color="#0047ab") +
  coord_flip() +
  my_theme() +
  geom_text(aes(label=total_word_count),
            hjust=1.15,
            color="#f3faff",
            size=3) +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        plot.caption=element_text(face="italic"),
        axis.text.x=element_blank()) +
  labs(subtitle = "Longer books contain more words.",
       caption = "Data was mined from the gutenberg.org database using the gutenbergr package.",
       x="", y="") +
  ggtitle("Number of words in each book of the novel \n My Antonia (1918) by Willa Cather")


# (006) compare lexical diversity per book
lexical_diversity<-df %>% 
  group_by(book_name) %>% 
  unnest_tokens(input=text, output=word) %>% 
  anti_join(stop_words, by="word") %>% 
  count(book_name, word, sort=T) %>% 
  summarize(lex_diversity=n_distinct(word)) 
lexical_diversity

lexical_diversity %>% 
  ggplot(mapping=aes(x=reorder(book_name, lex_diversity), 
                     y = lex_diversity,
                     fill = book_name)) +
  geom_chicklet(width = 0.75) +
  scale_fill_rcolorUtrecht(palette = "hu") +
  coord_flip() +
  my_theme() +
  labs(x="", y="",
       subtitle="The longer the book, the higher the lexical diversity.",
       caption = "Data was mined from the gutenberg.org database using the gutenbergr package.") +
  geom_text(aes(label=lex_diversity), hjust=1.2, color="#f3faff", size=4) +
  ggtitle("Lexical diversity in each book of the novel\nMy Antonia (1918) by Willa Cather") +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        # axis.text.y=element_blank(),
        plot.caption=element_text(face="italic"))


# (007) most common words
df %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  filter(!word=="house",
         !word=="head",
         !word=="don’t") %>% 
  count(word, sort = T) %>% 
  head(n = 15) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_chicklet(width = 0.7,
                fill="#0047ab", color="#0047ab") +
  coord_flip() +
  geom_text(aes(label=n), 
            hjust=1.1, 
            color="antiquewhite", 
            size=3.5) +
  my_theme() +
  labs(x="", y="",
       subtitle="Who shaped Jim's memory of his youth on the plains? The pioneer women of course.",
       caption = "Data was mined from the gutenberg.org database using the gutenbergr package.") +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        plot.caption=element_text(face="italic")) +
  ggtitle("Top 15 most used words in the novel\nMy Antonia (1918) by Willa Cather")


# (008) most common bigrams
bigrams<-df %>% 
  group_by(book_name) %>% 
  unnest_tokens(bigram, text, token="ngrams", n=2) %>% 
  count(bigram, sort=T)

bigrams_united<-bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(bigram, c(word1, word2), sep = " ")

bigrams_united %>% 
  group_by(book_name) %>% 
  head(15) %>% 
  ggplot(aes(reorder(bigram, n), n, fill=book_name)) + 
  geom_chicklet(width=0.75,
                radius=unit(2,"mm")) +
  coord_flip() + 
  my_theme() + scale_fill_rcolorUtrecht(palette="hu") +
  labs(x="", y="",
       caption="Data was mined from the gutenberg.org database using the gutenbergr package.") +
  ggtitle("The most used bigrams in the novel\nMy Antonia (1918) by Willa Cather") +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        plot.caption=element_text(face="italic"))


# (009) negative and postive words
neg_pos_plot<-df %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords(), by="word") %>% 
  filter(!word=="like",
         !word=="burden",
         !word=="well") %>% 
  inner_join(get_sentiments("bing"), by="word") %>% 
  count(word, sentiment, sort = T) %>% 
  # ungroup() %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_chicklet(width = 1,
                radius=unit(2,"mm")) +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  my_theme() +
  geom_text(aes(label=n), hjust=1.5, color="#f3faff", size=3.5) +
  labs(x="", y="",
       subtitle="Which words make the novel nostalgic?",
       caption = "Data was mined from the gutenberg.org database using the gutenbergr package.") +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        plot.caption=element_text(face="italic")) +
  ggtitle("Most used negative and positive words in the novel\nMy Antonia (1918) by Willa Cather")

neg_pos_plot + scale_fill_rcolorUtrecht(palette = "hu")


# (010) negative and positive words per book
neg_pos_0<-df %>% 
  filter(book_name=="Introduction") %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!word=="like",
         !word=="burden",
         !word=="well") %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  group_by(sentiment) %>% 
  head(10) %>%
  # top_n(4) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_chicklet(width = 0.7,
                radius=unit(2,"mm"),
                fill="#0047ab", color="#0047ab") +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  my_theme() +
  geom_text(aes(label=n), hjust=1.5, color="#f3faff", size=3.5) +
  labs(x="", y="",
       caption = "Data was mined from the gutenberg.org database using the gutenbergr package.") +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        plot.caption=element_text(face="italic")) +
  ggtitle("Most used negative and positive words in the introduction")
neg_pos_0


neg_pos_1<-df %>% 
  filter(book_name=="Book 1: The Shimerdas") %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!word=="like",
         !word=="burden",
         !word=="well") %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  # ungroup() %>% 
  group_by(sentiment) %>% 
  # head(10) %>% 
  top_n(5) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_chicklet(width = 0.7,
                radius=unit(2,"mm"),
                fill="#0047ab", color="#0047ab") +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  my_theme() +
  geom_text(aes(label=n), hjust=1.5, color="#f3faff", size=3.5) +
  labs(x="", y="",
       caption = "Data was mined from the gutenberg.org database using the gutenbergr package.") +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        plot.caption=element_text(face="italic")) +
  ggtitle("Most used negative and positive words in Book 1: The Shimerdas")
neg_pos_1


neg_pos_2<-df %>% 
  filter(book_name=="Book 2: The Hired Girls") %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!word=="like",
         !word=="burden",
         !word=="well") %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  # ungroup() %>% 
  group_by(sentiment) %>% 
  # head(10) %>% 
  top_n(5) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_chicklet(width = 0.7,
                radius=unit(2,"mm"),
                fill="#0047ab", color="#0047ab") +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  my_theme() +
  geom_text(aes(label=n), hjust=1.5, color="#f3faff", size=3.5) +
  labs(x="", y="",
       caption = "Data was mined from the gutenberg.org database using the gutenbergr package.") +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        plot.caption=element_text(face="italic")) +
  ggtitle("Most used negative and positive words in Book 1: The Hired Girls")
neg_pos_2


neg_pos_3<-df %>% 
  filter(book_name=="Book 3: Lena Lingard") %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!word=="like",
         !word=="burden",
         !word=="well") %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  # ungroup() %>% 
  group_by(sentiment) %>% 
  # head(10) %>% 
  top_n(5) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_chicklet(width = 0.7,
                radius=unit(2,"mm"),
                fill="#0047ab", color="#0047ab") +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  my_theme() +
  geom_text(aes(label=n), hjust=1.5, color="#f3faff", size=3.5) +
  labs(x="", y="",
       caption = "Data was mined from the gutenberg.org database using the gutenbergr package.") +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        plot.caption=element_text(face="italic")) +
  ggtitle("Most used negative and positive words in Book 3: Lena Lingard")
neg_pos_3


neg_pos_4<-df %>% 
  filter(book_name=="Book 4: The Pioneer Woman's Story") %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!word=="like",
         !word=="burden",
         !word=="well") %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  # ungroup() %>% 
  group_by(sentiment) %>% 
  # head(10) %>% 
  top_n(5) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_chicklet(width = 0.7,
                radius=unit(2,"mm"),
                fill="#0047ab", color="#0047ab") +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  my_theme() +
  geom_text(aes(label=n), hjust=1.5, color="#f3faff", size=3.5) +
  labs(x="", y="",
       caption = "Data was mined from the gutenberg.org database using the gutenbergr package.") +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        plot.caption=element_text(face="italic")) +
  ggtitle("Most used negative and positive words in Book 3: Lena Lingard")
neg_pos_4


neg_pos_5<-df %>% 
  filter(book_name=="Book 5: Cuzak's Boys") %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!word=="like",
         !word=="burden",
         !word=="well") %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  # ungroup() %>% 
  group_by(sentiment) %>% 
  # head(10) %>% 
  top_n(5) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_chicklet(width = 0.7,
                radius=unit(2,"mm"),
                fill="#0047ab", color="#0047ab") +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  my_theme() +
  geom_text(aes(label=n), hjust=1.5, color="#f3faff", size=3.5) +
  labs(x="", y="",
       caption = "Data was mined from the gutenberg.org database using the gutenbergr package.") +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        plot.caption=element_text(face="italic")) +
  ggtitle("Most used negative and positive words in Book 3: Lena Lingard")
neg_pos_5


# (011) sentiment analysis of the novel
df %>% 
  group_by(book_name, line_number) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(book_name, index = line_number %/% 80, sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(index, sentiment, fill = book_name)) +
  geom_col(width = 0.8) +
  ggtitle("Finding the emotional arc in the novel\nMy Antonia (1918) by Willa Cather") +
  my_theme() + scale_fill_rcolorUtrecht(palette="hu") +
  labs(subtitle="Descending lines mean a negative emotional index, nwhile ascending\nlines mean a positive emotional index.",
       caption = "Data was mined from the gutenberg.org database using the gutenbergr package.",
       y="Sentiment score", x="") +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        plot.caption=element_text(face="italic"))


# (012) comparing the 3 lexicons in a sentiment analysis
# Source: https://www.tidytextmining.com/sentiment.html

afinn<-df %>% 
  group_by(book_name, line_number) %>% 
  unnest_tokens(output=word, input=text) %>% 
  anti_join(stop_words, by="word") %>% 
  inner_join(get_sentiments("afinn"), by="word") %>% 
  group_by(index=line_number %/% 80) %>% 
  summarize(sentiment=sum(value)) %>% 
  mutate(method="AFINN")

bing_and_nrc<-bind_rows(
  df %>%
    group_by(book_name, line_number) %>% 
    unnest_tokens(output=word, input=text) %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  df %>% 
    group_by(book_name, line_number) %>% 
    unnest_tokens(output=word, input=text) %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = line_number %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE, width=0.7) +
  facet_wrap(~method, ncol = 1, scales = "free_y") +
  my_theme() + scale_fill_rcolorUtrecht(palette="hu") +
  labs(y="Sentiment score", x="",
       subtitle="The AFINN lexicon shows a greater negative emotional index compared to Bing and NRC.",
       caption = "Data was mined from the gutenberg.org database using the gutenbergr package.") +
  ggtitle("Comparing three emotion lexicons to find the emotional arc of\nthe novel My Antonia (1918) by Willa Cather") +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        plot.caption=element_text(face="italic"))


# (013) word correlations
# Source: https://bookdown.org/Maxine/tidy-text-mining/counting-and-correlating-pairs-of-words-with-widyr.html

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
  geom_chicklet(width = 0.7,
                fill="#0047ab", color="#0047ab") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() + my_theme() +
  labs(x="", y="Correlation",
       subtitle="What is the first thing you think of when hearing these words?",
       caption = "Data was mined from the gutenberg.org database using the gutenbergr package.") +
  ggtitle("Word correlations in the novel\nMy Antonia (1918) by Willa Cather") +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        plot.caption=element_text(face="italic"))


# (014) gender roles
# Source: https://www.r-bloggers.com/2017/04/gender-roles-with-text-mining-and-n-grams/
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
  filter(!word2=="is",
         !word2=="was",
         !word2=="were") %>% 
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
  labs(title = "Establishing gender roles in the novel\nMy Antonia by correlating gender associated words",
       caption = "Data was mined from the gutenberg.org database using the gutenbergr package.",
       subtitle = "Women have, turn and ask while men like, think and keep.",
       y = "", x = "") +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        plot.caption=element_text(face="italic")) +
  scale_color_rcolorUtrecht(palette = "hu")


# (014) table with sentence and word count
summary<-left_join(total_sentences, total_words, by="book_name")
summary