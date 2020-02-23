#Read in data

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

#Load packages

library(tidyverse)
library(countrycode)
library(ggthemes)
library(ggpubr)
library(gridExtra)

#data wrangling

food_consumption <- food_consumption %>%
  mutate(cat=ifelse(food_category %in% c("Beef","Fish","Lamb & Goat","Pork","Poultry", "Milk - inc. cheese", "Eggs"),
                    "Animal Products",
                    "Plant Based"))


#grouping by total animal/plant product emission

total_animal_products_emission <-food_consumption %>%
  group_by(country)%>%
  filter(cat == "Animal Products") %>%
  summarise(total_animal_emission = sum(co2_emmission)) %>%
  arrange(desc(total_animal_emission))

total_plant_products_emission <- food_consumption %>%
  group_by(country)%>%
  filter(cat == "Plant Based") %>%
  summarise(total_plant_emission = sum(co2_emmission)) %>%
  arrange(desc(total_plant_emission))

#grouping by total animal/plant product consumption

total_animal_products_consumption <-food_consumption %>%
  group_by(country)%>%
  filter(cat == "Animal Products") %>%
  summarise(total_animal_consumption = sum(consumption)) %>%
  arrange(desc(total_animal_consumption))

total_plant_products_consumption <- food_consumption %>%
  group_by(country)%>%
  filter(cat == "Plant Based") %>%
  summarise(total_plant_consumption = sum(consumption)) %>%
  arrange(desc(total_plant_consumption))


#join tables

total_table_emission <- full_join(total_animal_products_emission, total_plant_products_emission)
total_table_consumption <-full_join(total_animal_products_consumption, total_plant_products_consumption)

total_overall <-full_join(total_table_consumption, total_table_emission)

#add continents

total_overall <- total_overall %>%
  mutate(continent = countrycode(country, 'country.name', 'continent'))  

#add total consumption

total_overall <- total_overall %>%
  mutate(total_consumption = total_animal_consumption + total_plant_consumption)

#add total emission

total_overall <- total_overall %>%
  mutate(total_emission = total_animal_emission + total_plant_emission)

#percentage plant based consumption

total_overall <- total_overall %>%
  mutate(percent_plant = ((total_plant_consumption/total_consumption)* 100))

#percentage animal based consumption

total_overall <- total_overall %>%
  mutate(percent_animal = ((total_animal_consumption/total_consumption)* 100))
         

#graph both animal product plot and plant plot

animal_plot <- ggplot(total_overall,
                      aes(percent_animal, 
                          total_emission, 
                          colour = factor(continent))) + geom_point() + scale_x_continuous(limits=c(0,100)) +
  labs(x="Animal Product Consumption kg/person/year (%)", y="Total Carbon Dioxide Emissions (kg/person/year)") + 
  theme_pubr() + theme(legend.title = element_blank())
 


plant_plot <- ggplot(total_overall, 
                     aes(percent_plant,
                         total_emission,
                         colour = factor(continent))) + geom_point() + scale_x_continuous(limits=c(0,100)) +
  labs(x="Plant Product Consumption kg/person/year (%)", y="Total Carbon Dioxide Emissions (kg/person/year)") + 
  theme_pubr() +
  theme(legend.title = element_blank())


print(animal_plot)
print(plant_plot)

grid.arrange(animal_plot,
             plant_plot,
             top = "The effect of the percentage of either plant or animal based food product consumption on total overall CO2 emissions of 130 countries",
             bottom = textGrob("TidyTuesday submission by Elizabeth Beales. Data from https://github.com/rfordatascience/tidytuesday"))




