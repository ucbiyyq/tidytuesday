library(tidyverse)
library(lubridate)
library(here)
library(gganimate)

# Get the Data -------------------------

rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

# IF YOU USE THIS DATA PLEASE BE CAUTIOUS WITH INTERPRETATION
nasa_fire <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/MODIS_C6_Australia_and_New_Zealand_7d.csv')

# For JSON File of fires
url <- "http://www.rfs.nsw.gov.au/feeds/majorIncidents.json"

aus_fires <- sf::st_read(url)


# explore ----------------------

rainfall %>% nrow()
rainfall %>% colnames()
rainfall %>% sample_n(5)
rainfall %>% count(period)
rainfall %>% filter(city_name == "Perth" & year == "1967" & month == "04" & day == "25")

temperature %>% nrow()
temperature %>% colnames()
temperature %>% sample_n(5)
temperature %>% count(date)
temperature %>% count(city_name)
temperature %>% count(temperature)
temperature %>% count(temp_type)
temperature %>% count(site_name)
temperature %>% filter(date == "1967-04-25" & city_name == "PERTH")


nasa_fire %>% nrow()
nasa_fire %>% colnames()
nasa_fire %>% sample_n(5)

aus_fires %>% nrow()
aus_fires %>% colnames()
aus_fires %>% sample_n(1)


# let's see if there's a pattern between rainfall and temperature

# prep rainfall data =========================
# ... I guess we ignore period and quality, not sure what they signify
# ... also only filtering for the non-null rainfall measurements
t1 <- rainfall %>% 
    select(
        city_name
        , year
        , month
        , day
        , rainfall
    ) %>% 
    mutate(
        city_name = tolower(city_name)
        , date = ymd(str_c(year, month, day, sep = "-"))
    ) %>% 
    select(
        city_name
        , date
        , rainfall
    )

# there's an odd periodicity to the missing rainfall measurements
# ... if we had a domain level expert, I'd ask ... but for now will just have to drop any na records
t1 %>% ggplot(aes(date, is.na(rainfall))) + geom_jitter(alpha = 0.01)

t2 <- t1 %>% filter(!is.na(rainfall))
rf <- t2


# prep temperature data =========================
t1 <- temperature %>% 
    select(
        city_name
        , date
        , temperature
        , temp_type
    ) %>% 
    mutate(
        city_name = tolower(city_name)
    ) %>% 
    pivot_wider(names_from = temp_type, values_from = temperature)
tp <- t1


# prep combo ================
# ... scale the measures by city
# see https://stackoverflow.com/questions/35775696/trying-to-use-dplyr-to-group-by-and-apply-scale
scale_this <- function(x){
    # (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
    (x - min(x)) / (max(x) - min(x))
}

t1 <- rf %>% 
    inner_join(tp, by = c("city_name", "date")) %>% 
    filter(!is.na(rainfall), !is.na(max), !is.na(min)) %>% 
    mutate(
        yr = as.integer( format(date, "%Y") )
    ) %>% 
    select(city_name, yr, everything(), -date) %>% 
    group_by(city_name, yr) %>% 
    summarize(
        rainfall = mean(rainfall)
        , min = mean(min)
        , max = mean(max)
    ) %>% 
    ungroup() %>% 
    # group_by(city_name) %>% 
    mutate(
        rainfall = scale_this(rainfall)
        , min = scale_this(min)
        , max = scale_this(max)
    )
combo <- t1

# ... min temps vs max temps
combo %>% 
    ggplot() +
    geom_point(aes(min, max), alpha = 0.01) + 
    facet_wrap(vars(city_name))
# ... some correlation here, as to be expected

# ... rain vs min temps
combo %>% 
    ggplot() +
    geom_point(aes(rainfall, min), alpha = 0.01) + 
    facet_wrap(vars(city_name))
# ... nope


# ... rain vs max temps
combo %>% 
    ggplot() +
    geom_point(aes(rainfall, max), alpha = 0.01) + 
    facet_wrap(vars(city_name))

# ... also nope



# ... rain vs rng
combo %>% 
    ggplot() +
    geom_point(aes(rainfall, rng), alpha = 0.01) + 
    facet_wrap(vars(city_name))
# ... nope



# what if we just used gganimate for fun?

p1 <- combo %>% 
    ggplot(aes(x = min, y = max, size = rainfall, color = city_name)) + 
    geom_point()
    
a1 <- p1 + 
    transition_time(yr) + 
    # shadow_mark(alpha = 0.3, size = 0.5)
    shadow_mark(alpha = 0.3)
    # shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(a1, fps=10)