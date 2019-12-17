library(here)
library(janitor)
library(tidyverse)

# get data ------------

# see https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-12-17

dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')


# eda -------------

dog_moves %>% View()
dog_moves %>% nrow() # 90 rows
dog_moves %>% colnames()
dog_moves %>% str()
sapply(dog_moves, function(x) sum(is.na(x))) %>% enframe()
dog_moves %>% count(location, sort = TRUE)
dog_moves %>% janitor::get_dupes(location) # no dup locations
dog_moves %>% pull(exported) %>% hist()
dog_moves %>% pull(exported) %>% summary()
dog_moves %>% pull(imported) %>% hist()
dog_moves %>% pull(imported) %>% summary()
dog_moves %>% pull(total) %>% hist()
dog_moves %>% pull(total) %>% summary()
dog_moves %>% count(inUS, sort = TRUE)




dog_travel %>% View()
dog_travel %>% colnames()
t1 <- dog_travel %>% get_dupes() # 706 dups across all fields
t1 <- dog_travel %>% get_dupes(description) # 3450 dups in description??? 
# ... that's a bit more than half of travel records are dups
# ... or some descriptions are just very common, maybe the shelter likes to reuse them
# ... some descriptions do appear to be advertisements
t1 <- dog_travel %>% 
    count(description, sort = TRUE) %>% 
    filter(n > 1)

sapply(dog_travel, function(x) sum(is.na(x))) %>% enframe()
dog_travel %>% count(id, sort = TRUE) # even dups in id ...
dog_travel %>% count(contact_city, sort = TRUE)
dog_travel %>% count(contact_state, sort = TRUE)
dog_travel %>% count(found, sort = TRUE)
dog_travel %>% count(manual, sort = TRUE)
dog_travel %>% count(remove, sort = TRUE)
dog_travel %>% count(still_there, sort = TRUE)



dog_descriptions %>% View()
dog_descriptions %>% colnames()
dog_descriptions %>% get_dupes() # no dups!
dog_descriptions %>% get_dupes(id) # no dups
sapply(dog_descriptions, function(x) sum(is.na(x))) %>% enframe()

dog_descriptions %>% count(status, sort = TRUE) # looks like there's something odd happening with this data field. Bad field?
dog_descriptions %>% select(description) %>% slice(1) %>% pull()
dog_descriptions %>% count(species) # all dogs =)
dog_descriptions %>% count(breed_primary) # wow, couple of hundred breeds??? who knews dogs had so many
dog_descriptions %>% count(breed_secondary) # same here
dog_descriptions %>% count(breed_mixed) # about pure-to-mixed ratio is about 2.5 
dog_descriptions %>% count(breed_unknown) # all breeds known
dog_descriptions %>% count(color_primary) # 16 kinds of colors
dog_descriptions %>% count(color_secondary) # 16 kinds of colors
dog_descriptions %>% count(color_tertiary)

library(summarytools)
view(summarytools::dfSummary(dog_descriptions))


# nlp on dog_travels -----------------------

# note, dog_travel appears to have some dups accross various columns
# so for some tweaking needed to correct for this
dog_travel_unique <- dog_travel %>% 
    select(id, contact_state, description) %>%
    mutate(contact_state = case_when(
        contact_state == "17325" ~ "PA"
        , TRUE ~ contact_state
    )) %>% 
    distinct()
    

dog_travel_unique %>% get_dupes() # should be no dups
dog_travel_unique %>% get_dupes(id) # should be no dups
dog_travel_unique %>% count(contact_state, sort = TRUE)
dog_travel_unique %>% count(contact_state) %>% arrange(n)

dog_travel_descriptions <- dog_travel_unique %>% 
    select(description)




# nlp on dog_descriptions -------------------


dogs_children <- dog_descriptions %>% 
    filter(!is.na(env_children)) %>% 
    select(id, description, env_children)

dogs_children %>% get_dupes() # no dups
dogs_children %>% get_dupes(id) # no dups

# ... is there a difference in how dogs allowed to be with children are described?

dogs_children %>% 
    filter(env_children == TRUE) %>% 
    sample_n(1) %>% 
    pull(description)

dogs_children %>% 
    filter(env_children == FALSE) %>% 
    sample_n(1) %>% 
    pull(description)


# tidy the descriptions -----------------
# note, the unnest function appears to take care of the \n
# but sometimes have to do quick and dirty cleanup, i.e.
# ... the â character shows up, which is probably supposed to be a space
# ... some sentences end with "word.word", instead of "word. word"
# ... some tokens are separated by underscore, period, or colon (key:value)

library(tidytext)
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


tidy_dogs_1g %>% filter(str_detect(word, "help")) %>% count(word) %>% View()
tidy_dogs_1g %>% filter(str_detect(word, "â")) %>% count(word) %>% View()
tidy_dogs_1g %>% filter(str_detect(word, "'s")) %>% count(word) %>% View() # lots of possesives, need to clean this up too

# finds word tokens that don't have standard characters or numbers
tidy_dogs_1g %>% filter(str_detect(word, "[^[a-z0-9]]")) %>% count(word, sort = TRUE) %>% View()
# ... looks like we might need some better regex skills to clean up some of the weird text
# ... such as inserting a space between two words with a period: "pet.smiling" => "pet. smiling"



# gets rid of the stopwords from our tidy data
data(stop_words)

tidy_dogs_1g <- tidy_dogs_1g %>% 
    anti_join(stop_words, by = "word")

tidy_dogs_1g %>% count(word, sort = TRUE)
tidy_dogs_1g %>% count(word, sort = TRUE) %>% pull(n) %>% summary()
tidy_dogs_1g %>% count(word, sort = TRUE) %>% pull(n) %>% hist()

# most common words
tidy_dogs_1g %>% 
    count(word, sort = TRUE) %>% 
    rename(n_dogs = n) %>% 
    top_n(n = 10, wt = n_dogs) %>% 
    mutate(word = reorder(word, n_dogs)) %>% 
    ggplot(aes(word, n_dogs)) + 
    geom_col() +
    xlab(NULL) + 
    coord_flip()


# most common words on env_children
# see https://juliasilge.com/blog/reorder-within/
tidy_dogs_1g %>% 
    count(word, env_children, sort = TRUE) %>% 
    mutate(env_children = case_when(
        env_children == TRUE ~ "recommended"
        , TRUE ~ "not recommended"
    )) %>% 
    group_by(env_children) %>% 
    top_n(n = 10, wt = n) %>% 
    ungroup() %>% 
    mutate(
        env_children = as.factor(env_children)
        , word = reorder_within(word, n, env_children)
    ) %>% 
    ggplot(aes(x = word, y = n, fill = env_children)) + 
    geom_col(show.legend = FALSE) +
    labs(
        title = "Common words describing dogs recommended for a home with children"
        , x = NULL 
        , y = "number of time word appears"
        , caption = "data from: The Pudding"
    ) + 
    coord_flip() + 
    scale_x_reordered() + 
    facet_wrap(vars(env_children), scales = "free")



# what about tf-idf? -----------------
# see chap 3 of tidytext book
# note, instead of group by book and word, we care about the env_children and word

dog_words <- tidy_dogs_1g %>% 
    count(env_children, word, sort = TRUE) 

total_words <- dog_words %>% 
    group_by(env_children) %>% 
    summarize(total = sum(n))

dog_words <- dog_words %>% 
    left_join(total_words, by = "env_children") 
    

freq_by_rank <- dog_words %>% 
    group_by(env_children) %>% 
    mutate(
        rnk = row_number()
        , term_freq = (n / total)
    ) %>% 
    ungroup()

# ... of course, the term frequency & ranking should agree with the raw counts
freq_by_rank %>%
    group_by(env_children) %>% 
    top_n(10, wt = term_freq)
    
# ... shows zipf law
freq_by_rank %>% 
    ggplot(aes(x = rnk, y = term_freq, color = env_children)) +
    geom_line() + 
    scale_x_log10() +
    scale_y_log10()

rank_subset <- freq_by_rank %>% filter(rnk < 1000 & rnk > 10) # middle third
m1 <- lm(log10(term_freq) ~ log10(rnk), data = rank_subset)
m1
m1$coefficients[1] # y-intercept
m1$coefficients[2] # slope of line, which is the effect of log10(rnk)

# .. add the fitten power law
freq_by_rank %>% 
    ggplot(aes(x = rnk, y = term_freq, color = env_children)) +
    geom_line() + 
    geom_abline(intercept = m1$coefficients[1], slope = m1$coefficients[2], color = "gray50", linetype = 2) +
    scale_x_log10() +
    scale_y_log10()


# ... using tidytext helper function
dog_words_tfidf <- dog_words %>% 
    bind_tf_idf(word, env_children, n)

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
# ... twitter likes 16:9 aspect ratio

# what about bi-grams
tidy_dogs_2g <- dogs_children %>% 
    select(id, env_children, description) %>% 
    mutate(
        description = str_replace_all(description, "[Ââ]", " ")
        , description = str_replace_all(description, "_", " ")
        , description = str_replace_all(description, "\\.", " ")
        , description = str_replace_all(description, ":", " ")
    ) %>%
    unnest_tokens(bigram, description, token = "ngrams", n = 2)

# most common bigrams
tidy_dogs_2g %>% 
    count(bigram, sort = TRUE) %>% 
    rename(n_docs = n) %>% 
    top_n(n = 10, wt = n_docs) %>% 
    mutate(bigram = reorder(bigram, n_docs)) %>% 
    ggplot(aes(bigram, n_docs)) + 
    geom_col() +
    xlab(NULL) + 
    coord_flip()
