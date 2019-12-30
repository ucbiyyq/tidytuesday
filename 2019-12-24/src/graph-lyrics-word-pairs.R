library(here)
library(tidytext)
library(tidyverse)
library(widyr)


# get data ------------

# see https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-12-24

christmas_songs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_songs.csv")
christmas_lyrics <- readr::read_tsv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_lyrics.tsv")

# see book,
# Text Mining with R: a Tidy Approach
# chap 8
# can we vizualize the words in songs that tend to co-occur with each other?

# ... we want to join by song year and lyric
t1 <- christmas_songs %>% select(songid, song, year)
t2 <- christmas_lyrics %>% select(songid, lyric)
t3 <- t1 %>% inner_join(t2, by = "songid")
xmas <- t3

# ... note, the lyrics are split out into lines, which we don't care about
# ... we just want each lyric-word as a token per row
t1 <- xmas
t2 <- t1 %>% unnest_tokens(word, lyric) %>% distinct()
t3 <- t2 %>% anti_join(stop_words, by = "word")
xmas_tidy <- t3

# ... most common co-ocurrences per song?
t1 <- xmas_tidy %>% select(song, word)
t2 <- t1 %>% widyr::pairwise_count(word, song, sort = TRUE)
xmas_pairs <- t2

# ... to visualize as a graph
library(ggplot2)
library(igraph)
library(ggraph)

set.seed(20191230)
p1 <- xmas_pairs %>% 
    filter(n > 2) %>% 
    graph_from_data_frame() %>% 
    ggraph(layout = "fr") + 
    geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_color = "cyan4") +
    geom_node_point(size = 5) + 
    geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) + 
    guides(
        edge_alpha = guide_legend(title = "# Pairs")
        , edge_width = guide_legend(title = "# Pairs")
    ) + 
    labs(
        title = "Words associated with each other in Christmas songs"
        # , subtitle = "not many common pairs, since song lyrics tend to be unique"
        , caption = "data from: Kaggle, Billboard Top 100 Christmas Carol Dataset"
    ) + 
    theme_void() + 
    theme( plot.margin = grid::unit(c(.1, .1, .1, .1), "in") )

p1 %>% ggsave(
    filename = "xmas_lyric_word_pairs.png"
    , device = "png"
    , path = here("2019-12-24", "reports", "figures")
    , width = 8
    , height = 4.4
    , dpi = 300
)

