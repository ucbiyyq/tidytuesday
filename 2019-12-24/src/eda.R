library(here)
library(janitor)
library(tidytext)
library(tidyverse)
library(summarytools)
library(widyr)


# get data ------------

# see https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-12-24

christmas_songs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_songs.csv")
christmas_lyrics <- readr::read_tsv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_lyrics.tsv")


# eda -------------

christmas_songs %>% nrow() # 387
christmas_songs %>% View() # stats about the song
christmas_songs %>% str()
christmas_songs %>% colnames()
christmas_songs %>% filter(weekid == "12/17/1960") %>% View()
christmas_songs %>% count(year)


view(summarytools::dfSummary(christmas_songs))


christmas_lyrics %>% nrow() # 7525
christmas_lyrics %>% View() # bunch of song lyrics
christmas_lyrics %>% colnames()
christmas_lyrics %>% select(lyric)
christmas_lyrics %>% select(year)

view(summarytools::dfSummary(christmas_lyrics))


# see book,
# Text Mining with R: a Tidy Approach
# chap 8
# have the pairing of words changed in the lyrics from year to year?
# we want to visualize the key words in the songs as network graph, which changes from year to year

# ... we want to join by song year and lyric
t1 <- christmas_songs %>% select(songid, song, year)
t2 <- t1 %>% mutate(decade = year - year %% 10)
t3 <- christmas_lyrics %>% select(songid, lyric)
t4 <- t2 %>% inner_join(t3, by = "songid")
xmas <- t4

# ... note, the lyrics are split out into lines, which we don't care about
# ... we just want each lyric-word as a token per row
t1 <- xmas
t2 <- t1 %>% unnest_tokens(word, lyric) %>% distinct()
t3 <- t2 %>% anti_join(stop_words, by = "word")
xmas_tidy <- t3

# ... most common words?
xmas_tidy %>% count(word, sort = TRUE)

# ... most common co-ocurrences per song?
t1 <- xmas_tidy %>% select(song, word)
t2 <- t1 %>% widyr::pairwise_count(word, song, sort = TRUE)
xmas_pairs <- t2

xmas_pairs %>% pull(n) %>% hist()
xmas_pairs %>% pull(n) %>% summary()
xmas_pairs %>% top_frac(.01, wt = n)
xmas_pairs %>% filter(n > 3)



# ... to visualize as a graph
library(ggplot2)
library(igraph)
library(ggraph)

set.seed(20191230)
xmas_pairs %>% 
    # top_frac(.01, wt = n) %>%
    filter(n > 2) %>% 
    graph_from_data_frame() %>% 
    ggraph(layout = "fr") + 
    geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_color = "cyan4") +
    geom_node_point(size = 5) + 
    geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) + 
    theme_void()


# ... most common co-ocurrences per song, per year?
pairwise_per_year <- function(xmas_tidy) {
    # gets unique years
    yrs <- xmas_tidy %>% pull(year) %>% unique()
    
    # creates an empty dataframe to hold our results
    xmas_pairs_yrs <- tibble(
        item1 = character()
        , item2 = character()
        , n = double()
        , year = double()
    )
    
    # for each year, does the pairwise count for each song
    for(yr in yrs) {
        t1 <- xmas_tidy %>%
            filter(year == yr) %>%
            # filter(year == 1958) %>%
            select(song, word)
        t2 <- t1 %>% 
            widyr::pairwise_count(word, song, sort = TRUE)
        t3 <- t2 %>% 
            mutate(year = yr)
            # mutate(year = 1958)
        xmas_pairs_yrs <- bind_rows(
            xmas_pairs_yrs
            , t3
        )
    }
    return(xmas_pairs_yrs)    
}


xmas_pairs_yrs <- pairwise_per_year(xmas_tidy)
xmas_pairs_yrs %>% View()
xmas_pairs_yrs %>% group_by(year) %>% summarize(min_n = min(n), med_n = median(n), max_n = max(n))
# ... actually this looks way too sparse, repeating pairs of words are quite rare in lyrics, which I guess means the songs don't tend to repeat
# ... maybe lets try decades




# ... most common co-ocurrences per song, per year?
pairwise_per_decade <- function(xmas_tidy) {
    # gets unique years
    decs <- xmas_tidy %>% pull(decade) %>% unique()
    
    # creates an empty dataframe to hold our results
    xmas_pairs_decs <- tibble(
        item1 = character()
        , item2 = character()
        , n = double()
        , decade = double()
    )
    
    # for each year, does the pairwise count for each song
    for(dec in decs) {
        t1 <- xmas_tidy %>%
            filter(decade == dec) %>%
            select(song, word)
        t2 <- t1 %>% 
            widyr::pairwise_count(word, song, sort = TRUE)
        t3 <- t2 %>% 
            mutate(decade = dec)
        xmas_pairs_decs <- bind_rows(
            xmas_pairs_decs
            , t3
        )
    }
    return(xmas_pairs_decs)
}

xmas_pairs_decs <- pairwise_per_decade(xmas_tidy)
xmas_pairs_decs %>% View()
xmas_pairs_decs %>% group_by(decade) %>% summarize(min_n = min(n), med_n = median(n), max_n = max(n))
# ... ok still too sparse ... well it looks like the word-pairs per song are quite unique even by decades

