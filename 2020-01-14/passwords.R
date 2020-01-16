library(tidyverse)
library(lubridate)
library(modelr)
library(xkcd)
library(extrafont)


# Get the Data

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

summarytools::view(summarytools::dfSummary(passwords))

z_it <- function(x) {
    z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    return(z)
}

s_it <- function(x) {
    s <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    return(s)
}


t1 <- passwords %>% 
    filter(!is.na(password)) %>% 
    mutate(
        online_crack_sec = case_when(
            time_unit == "years" ~ value * 3.154e+7
            , time_unit == "months" ~ value * 2628336.2137829
            , time_unit == "weeks" ~ value * 60 * 60 * 24 * 7
            , time_unit == "days" ~ value * 60 * 60 * 24
            , time_unit == "hours" ~ value * 60 * 60
            , time_unit == "minutes" ~ value * 60
            , time_unit == "seconds" ~ value
            , TRUE ~ NA_real_
        )
        , length = str_length(password)
    ) %>% 
    select(-value, -time_unit) %>% 
    mutate(
        rank_z = z_it(rank)
        , online_crack_z = z_it(online_crack_sec)
        , offline_crack_z = z_it(offline_crack_sec)
        , strength_z = z_it(strength)
        , length_z = z_it(length)
    ) %>% 
    mutate(
        rank_s = s_it(rank)
        , online_crack_s = s_it(online_crack_sec)
        , offline_crack_s = s_it(offline_crack_sec)
        , strength_s = s_it(strength)
        , length_s = s_it(length)
    )




t1 %>% 
    select(online_crack_z, offline_crack_z, strength_z, rank_z, length_z) %>% 
    pairs()


t1 %>% 
    select(online_crack_s, offline_crack_s, strength_s, rank_s, length_s) %>% 
    pairs()


t1 %>% 
    filter(category == "animal") %>% 
    select(online_crack_z, offline_crack_z, strength_z, rank_z, length_z) %>% 
    pairs()




# R for DS, chap 25
pass_model <- function(df) {
    lm(strength_z ~ length_z, data = df)
}

by_cat <- t1 %>% 
    group_by(category) %>% 
    nest()

# models <- map(by_cat$data, pass_model)

by_cat_m <- by_cat %>% 
    mutate(model = map(data, pass_model))

by_cat_m_2 <- by_cat_m %>% 
    mutate(
        resids = map2(data, model, add_residuals)
    )

resids  <- by_cat_m_2 %>% 
    unnest(resids) %>% 
    select(category, length_z, resid)

resids %>% 
    ggplot(aes(length_z, resid)) + 
    geom_jitter(aes(group = category)) +
    facet_wrap(vars(category))



xrange <- range(t1$length_z)
yrange <- range(t1$strength_z)
t1 %>% 
    select(category, length_z, strength_z) %>% 
    ggplot(aes(length_z, strength_z)) +
    geom_jitter() + 
    geom_smooth(method="lm", se = FALSE, color = "red") + 
    facet_wrap(vars(category)) +
    xkcd::theme_xkcd() +
    xkcd::xkcdaxis(xrange, yrange) + 
    labs(
        title = "increases in password length slightly increases strength"
        , subtitle = "except in some categories, where there are some short and strong ones"
        , x = "length z-score"
        , y = "strength z-score"
    )

    

xrange <- range(mtcars$mpg)
yrange <- range(mtcars$wt)
p <- ggplot() +
    geom_point(aes(mpg, wt), data=mtcars) +
    xkcd::xkcdaxis(xrange,yrange)
p
