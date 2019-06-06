
#Data####
RamenRaw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

#Data Dictionary####
#review_number	integer	Ramen review number, increasing from 1
#brand-	character	Brand of the ramen
#variety-	character	The ramen variety, eg a flavor, style, ingredient
#style-	character	Style of container (cup, pack, tray,
#country-	character	Origin country of the ramen brand
#stars-	double	0-5 rating of the ramen, 5 is best, 0 is worst
 
#Libraries####                                   

#devtools::install_github("dill/emoGG") 
library(tidyverse)
library(tidylog)
library(skimr)
library(visdat)
library(emoGG)

emoGG::emoji_search("ramen")
emoGG::geom_emoji()
#exploring
vis_miss(RamenRaw) #remove those with no ratings, v small percent; can also remove those w no style listed.
skim(RamenRaw)

RamenClean<- RamenRaw %>% 
  filter(is.na(stars) == F, is.na(style) == F) %>% 
  mutate(style = factor(style),
         country = factor(country))

#which countries produce the highest rated ramen
RamenClean %>% 
  group_by(country) %>% 
  summarize(AvgRating = mean(stars),
            NReviews = n()) %>% 
  ggplot(aes(y = reorder(country, AvgRating),x = AvgRating)) +
  geom_segment(aes(x = 0, xend = AvgRating, yend = country), color = "#e0dabc", size = 1.5) +
  geom_emoji(emoji = "1f365", size = .03) +
  geom_vline(xintercept = mean(RamenClean$stars), color = "red")+
  geom_text(aes(label = NReviews), size = 3, hjust = -.5) +
  labs(title = "Which Countries Produce the Best Ramen?",
       x = "Average Rating Across All Products (0-5)",
       caption = "Numbers represent total N of reviews per country\nData: TheRamenRater.com|Viz: @phillynerd") +
  scale_x_continuous(limits = c(0,5)) +
  add_emoji(emoji = "1f35c") +
  theme(panel.grid = element_blank(),
       panel.background = element_rect(fill = "#9b9999"),
       axis.title.y = element_blank() ) +
  annotate(geom = "text", 
           x = mean(RamenClean$stars), y = 0, 
           label = paste0("Overall Avg: ", round(mean(RamenClean$stars),1)),
           angle = 90,
           hjust = -.2, vjust = -.5, size = 4)
 
 
