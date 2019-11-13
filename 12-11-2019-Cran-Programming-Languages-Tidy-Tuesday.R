#load libraries

library(tidyverse)

#load data

cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

#take a quick look at the data
head(cran_code)
summary(cran_code)

#group data by language, summarise with total number of comments, then show the top 20 languages
most_comment <- cran_code %>% 
  group_by(language) %>%
  summarise(total_comment = sum(comment)) %>%
  filter(dense_rank(desc(total_comment)) <=20 )

#sort data
most_comment_sorted <- arrange(most_comment, desc(total_comment))

#create plot
plot <- ggplot(most_comment_sorted, aes(x = language, y = total_comment, fill = factor(language))) +
  geom_col() +
  ylab("Total Number of Comments") +
  xlab("Programming Language") +
  coord_flip() +
  theme_light() +
  theme(legend.position = "none") +
  ggtitle("The top 20 most commented programming languages within R packages")
  
plot
  
