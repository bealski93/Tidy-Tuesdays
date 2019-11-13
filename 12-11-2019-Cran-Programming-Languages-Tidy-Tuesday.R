library(tidyverse)
cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")
head(cran_code)
summary(cran_code)

most_comment <- cran_code %>% 
  group_by(language) %>%
  summarise(total_comment = sum(comment)) %>%
  filter(dense_rank(desc(total_comment)) <=20 )

most_comment_sorted <- arrange(most_comment, desc(total_comment))

plot <- ggplot(most_comment_sorted, aes(x = language, y = total_comment, fill = factor(language))) +
  geom_col() +
  ylab("Total Number of Comments") +
  xlab("Programming Language") +
  coord_flip() +
  theme_light() +
  theme(legend.position = "none") 
  
plot
  
