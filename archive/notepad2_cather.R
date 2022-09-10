library(gutenbergr)
library(tidyverse)
library(tidytext)
library(tidyr)
library(stringr)
library(ggchicklet)
library(extrafont)
library(jtools)
library(cowplot)
library(dutchmasters)

# make a custom theme
custom_theme <- function() {
  theme_apa(legend.pos = "none") +
    theme(panel.background = element_blank(),
          plot.background = element_rect(fill = "#fdf8f0"),
          panel.border = element_blank(),                     # facet border
          strip.background = element_blank(),                 # facet title background
          plot.margin = unit(c(.5, .5, .5, .5), "cm")) 
}


# get all (available) novels by Willa Cather from the Gutenberg database
gutenberg_works() %>% filter(author=="Cather, Willa")
cather <- gutenberg_download(c(24, 44, 94, 242, 2369, 13555),
                             mirror = "http://aleph.gutenberg.org",
                             meta_fields = "title")

lastbook <- gutenberg_download(c(65465))

temp <- right_join(cather, lastbook, by = "text")
temp

# sentence count per novel
cather %>% count(title)

# tidy data
cather_df <- cather %>% 
  mutate(line_number=row_number())
cather_df

words <- cather_df %>% 
  unnest_tokens(word, text)
words

words_tidy <- words %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(!word=="don’t",
         !word=="she’d",
         !word=="he’d",
         !word=="like") %>% 
  count(title, word, sort = T)
words_tidy

# check most common words per novel
top10_44 <- words_tidy %>% filter(title == "The Song of the Lark") %>% head(10)
top10_2369 <- words_tidy %>% filter(title == "One of Ours") %>% head(10)
top10_24 <- words_tidy %>% filter(title == "O Pioneers!") %>% head(10)
top10_94 <- words_tidy %>% filter(title == "Alexander's Bridge") %>% head(10)
top10_13555 <- words_tidy %>% filter(title == "Youth and the Bright Medusa") %>% head(10)
top10_242 <- words_tidy %>% filter(title == "My Antonia") %>% head(10)


plot_44 <- top10_44 %>% 
  ggplot(aes(reorder(word, n), n)) + geom_chicklet(fill = "#8b4513") + coord_flip() +
  custom_theme() + ggtitle("The 10 most occuring words in The Song of the Lark (1915)") +
  theme(text=element_text(family="mono"),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#8b4513")) + xlab("") + ylab("") +
  geom_text(aes(label=n), hjust=1.5, color="#fdf8f0", size=4.5)

plot_2369 <- top10_2369 %>% 
  ggplot(aes(reorder(word, n), n)) + geom_chicklet(fill = "#8b4513") + coord_flip() +
  custom_theme() + ggtitle("The 10 most occuring words in One of Ours (1922)") +
  theme(text=element_text(family="mono"),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#8b4513")) + xlab("") + ylab("") +
  geom_text(aes(label=n), hjust=1.5, color="#fdf8f0", size=4.5)

plot_24 <- top10_24 %>% 
  ggplot(aes(reorder(word, n), n)) + geom_chicklet(fill = "#8b4513") + coord_flip() +
  custom_theme() + ggtitle("The 10 most occuring words in O Pioneers! (1913)") +
  theme(text=element_text(family="mono"),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#8b4513")) + xlab("") + ylab("") +
  geom_text(aes(label=n), hjust=1.5, color="#fdf8f0", size=4.5)

plot_94 <- top10_94 %>% 
  ggplot(aes(reorder(word, n), n)) + geom_chicklet(fill = "#8b4513") + coord_flip() +
  custom_theme() + ggtitle("The 10 most occuring words in Alexander's Bridge (1912)") +
  theme(text=element_text(family="mono"),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#8b4513")) + xlab("") + ylab("") +
  geom_text(aes(label=n), hjust=1.5, color="#fdf8f0", size=4.5)

plot_13555 <- top10_13555 %>% 
  ggplot(aes(reorder(word, n), n)) + geom_chicklet(fill = "#8b4513") + coord_flip() +
  custom_theme() + ggtitle("The 10 most occuring words in Youth and the Bright Medusa (1920)") +
  theme(text=element_text(family="mono"),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#8b4513")) + xlab("") + ylab("") +
  geom_text(aes(label=n), hjust=1.5, color="#fdf8f0", size=4.5)

plot_242 <- top10_242 %>% 
  ggplot(aes(reorder(word, n), n)) + geom_chicklet(fill = "#8b4513") + coord_flip() +
  custom_theme() + ggtitle("The 10 most occuring words in My Ántonia (1918)") +
  theme(text=element_text(family="mono"),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#8b4513")) + xlab("") + ylab("") +
  geom_text(aes(label=n), hjust=1.5, color="#fdf8f0", size=4.5)

plot_grid(plot_94,
          plot_24,
          plot_44,
          plot_242,
          plot_13555,
          plot_2369,
          nrow = 3)


# sentiment analysis: bing
cather_bing <- cather %>% 
  mutate(line_number=row_number()) %>% 
  group_by(title, line_number) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(title, index = line_number %/% 100, sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(cather_bing, aes(index, sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x") +
  custom_theme() + scale_fill_dutchmasters(palette = "staalmeesters")

ggplot(cather_bing, aes(index, sentiment, fill = title)) + geom_chicklet() +
  facet_wrap(~title)

# sentiment analysis: nrc
cather_nrc <- cather %>% 
  mutate(line_number=row_number()) %>% 
  group_by(title, line_number) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(title, index = line_number %/% 100, sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)


cather_nrc %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(index, sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x") +
  custom_theme() + scale_fill_dutchmasters(palette = "staalmeesters")


# word count
cather %>% 
  group_by(title) %>% 
  unnest_tokens(word, text) %>% 
  count(title, word, sort = T) %>% 
  summarize(total_word_count = sum(n))

