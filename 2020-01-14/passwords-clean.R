library(tidyverse)
library(lubridate)
library(modelr)
library(xkcd)
library(extrafont)

# Get the Data

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# function to standardize a feature
z_it <- function(x) {
    z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    return(z)
}

# prep the data
t1 <- passwords %>% 
    filter(!is.na(password)) %>% 
    mutate(
        length = str_length(password)
    ) %>% 
    mutate(
        strength_z = z_it(strength)
        , length_z = z_it(length)
    )

# plot with xkcd theme
xrange <- range(t1$length_z)
yrange <- range(t1$strength_z)
p1 <- t1 %>% 
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

p1


# save plot as png
p1 %>% ggsave(
    filename = "passwords-length-strength.png"
    , device = "png"
    , path = here::here("2020-01-14")
    , width = 8
    , height = 4.18
    , dpi = 300
)
