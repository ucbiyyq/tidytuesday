library(dplyr)
library(ggplot2)

# gets data ----
fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')

# eda ----
fishing %>% colnames()
fishing %>% str()
fishing %>% count(year)
fishing %>% count(lake)
fishing %>% count(species)
fishing %>% count(grand_total)
fishing %>% count(comments)
fishing %>% count(region)
fishing %>% count(values)
fishing %>% select(year, grand_total, values) %>% summary()
fishing %>% select(year, grand_total, values) %>% purrr::map(~ sum(is.na(.)))

# yearly ----
# each row is an observation per region, so if we want yearly totals, we need to sum
# note, there are lots of missing values in the data files, so have to use na.rm = TRUE or sum function will drop values

yearly <- fishing %>% 
    select(year, lake, grand_total, values) %>% 
    group_by(year, lake) %>% summarize(grand_total = sum(grand_total, na.rm = TRUE), values = sum(values, na.rm = TRUE))

yearly %>% ggplot(aes(grand_total)) + geom_histogram()
yearly %>% ggplot(aes(values)) + geom_histogram()

yearly %>% ggplot(aes(grand_total, values, color = lake)) + geom_point()
yearly %>% ggplot(aes(year, grand_total, color = lake)) + geom_point()
yearly %>% ggplot(aes(year, values, color = lake)) + geom_point()

yearly %>% ggplot(aes(year, values, color = lake)) + geom_point() + facet_wrap(vars(lake))
