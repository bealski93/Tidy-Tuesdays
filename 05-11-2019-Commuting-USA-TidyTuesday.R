library(tidyverse)
library(maps)
library(ggmap)
library(ggthemes)

#load data
commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")


#geolocation data

commute_mode_cities <- 
  commute_mode %>%
  select(city,state) %>%
  unique() %>%
  mutate(location = paste(city, state, sep = "")) %>%
  mutate_geocode(location)

#combine tables to create dataset for the places where nobody walks to work

commute_mode_no_walking <-
  left_join(commute_mode, commute_mode_cities) %>% 
  filter(percent == 0,
         mode == "Walk")

commute_mode_no_biking <-
  left_join(commute_mode, commute_mode_cities) %>% 
  filter(percent == 0,
         mode == "Bike")

commute_mode_no_human_power <-
  full_join(commute_mode_no_biking,commute_mode_no_walking)

#plot graph

usa <- map_data("usa")
states_map <- map_data("state")

ggplot() + 
  geom_path(data=states_map, aes(x = long, y = lat, group = group, fill = "")) +
  geom_point(data=commute_mode_no_human_power, aes (x = lon, y = lat)) +
  
  theme_void() +
  labs(title = "US Cities where nobody commutes by bike or on foot",
       subtitle = "",
       caption = "")