library(here)
library(janitor)
library(tidytext)
library(tidyverse)

# gets data -------------------
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

# calculates the tfidf for descriptions & env_children ------------
dogs_children <- dog_descriptions %>% 
    filter(!is.na(env_children)) %>% 
    select(id, description, env_children)

# uni-grams
tidy_dogs_1g <- dogs_children %>% 
    select(id, env_children, description) %>% 
    mutate(
        description = str_replace_all(description, "[Ââ]", " ")
        , description = str_replace_all(description, "_", " ")
        , description = str_replace_all(description, "\\.", " ")
        , description = str_replace_all(description, ":", " ")
    ) %>%
    unnest_tokens(word, description) %>% 
    mutate(
        word = str_replace_all(word, "'s", "")
    )

# gets rid of stop words
data(stop_words)

tidy_dogs_1g <- tidy_dogs_1g %>% 
    anti_join(stop_words, by = "word")

# gets the tfidf
dog_words <- tidy_dogs_1g %>% 
    count(env_children, word, sort = TRUE) 

total_words <- dog_words %>% 
    group_by(env_children) %>% 
    summarize(total = sum(n))

dog_words <- dog_words %>% 
    left_join(total_words, by = "env_children")

dog_words_tfidf <- dog_words %>% 
    bind_tf_idf(word, env_children, n)

# glimpse the keywords
dog_words_tfidf %>% 
    select(-total) %>% 
    arrange(desc(tf_idf))


# ... visualization of tfidf ------------
p1 <- dog_words_tfidf %>% 
    mutate(env_children = case_when(
        env_children == TRUE ~ "recommended"
        , TRUE ~ "not recommended"
    )) %>% 
    mutate(
        env_children = as.factor(env_children)
        , word = reorder_within(word, n, env_children)
    ) %>% 
    group_by(env_children) %>% 
    top_n(n = 10, wt = tf_idf) %>% 
    ungroup() %>% 
    mutate(
        env_children = as.factor(env_children)
        , word = reorder_within(word, tf_idf, env_children)
    ) %>% 
    ggplot(aes(x = word, y = tf_idf, fill = env_children)) + 
    geom_col(show.legend = FALSE) +
    labs(
        title = "Keywords that describe dogs recommended for a home-with-children"
        , x = NULL 
        , y = "term-frequency inverse document frequency"
        , caption = "data from: The Pudding"
    ) + 
    coord_flip() + 
    scale_x_reordered() + 
    facet_wrap(vars(env_children), scales = "free")
p1 %>% ggsave(
    filename = "dogs_tfidf.png"
    , device = "png"
    , path = here("2019-12-17", "reports", "figures")
    , width = 8
    , height = 4.7
    , dpi = 72
)
