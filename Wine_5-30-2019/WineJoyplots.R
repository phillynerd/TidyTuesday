#Wine tasting tidy tuesday
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

#library
library(tidyverse)
library(skimr)
library(visdat)
library(ggridges)
library(magrittr)
library(paletteer)
#quick look
skim(wine_ratings)

#Data Dictionary####
#variable: class	description
#country:	character	Country of origin
#description:	character	Flavors and taste profile as written by reviewer
#designation:	character	The vineyard within the winery where the grapes that made the wine are from
#points:	double	The number of points WineEnthusiast rated the wine on a scale of 1-100 (though they say they only post reviews for wines that score >=80)
#price:	double	The cost for a bottle of the wine
#province:	character	The province or state that the wine is from
#region_1:	character	The wine growing area in a province or state (ie Napa)
#taster_name:	character	The taster/reviewer
#title:	character	The title of the wine review, which often contains the vintage (year)
#variety:	character	Grape type
#winery:	character	The winery that made the wine

#what varieties are in the data
wine_ratings %>% 
  group_by(variety) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(n >= 200) %>% 
  ggplot(aes(x = reorder(variety, n), y = n)) +
  geom_col() + coord_flip()

#How many unique tasters
wine_ratings %>% 
  group_by(taster_name) %>% 
  tally() %>% View() # 20 unique reviewers.  Drop Christina and Fiona, less than 5o reviews each

ExcludedReviewers <- c("Fiona Adams", "Christina Pickard")

#adding gender to wine tasters
wine_ratings %<>% 
  mutate(gender = ifelse(taster_name %in% c("Alexander Peartree", "Jeff Jenssen", "Jim Gordon",
                                            "Joe Czerwinski", "Matt Kettmann", "Michael Schachner",
                                            "Mike DeSimone", "Paul Gregutt", "Roger Voss", "Sean P. Sullivan"), 
                                            "Male", "Female")) #manual gender coding; looked up a few of the more ambiguous names to be sure
    
    

 #distribution of scores
wine_ratings %>% 
  filter((taster_name %in% ExcludedReviewers) == F, is.na(taster_name) == F) %>% 
  ggplot(aes(x = points, y = taster_name, fill = gender)) +
  geom_density_ridges() +
  geom_vline(xintercept = median(wine_ratings$points[(wine_ratings$taster_name %in% ExcludedReviewers) == F])) +
  annotate(geom = "text",label = "median", 
           x = median(wine_ratings$points[(wine_ratings$taster_name %in% ExcludedReviewers) == F]), 
           y = 0, 
           angle = 90,
           hjust = -.25,
           vjust = -.5) +
  labs(title = "The Joy(plot) of Wine Reviews",
       x = "Wine Score",
       y = "Reviewer",
       caption = "@phillynerd")

#what countries are most wines from
 wine_ratings %>% 
  filter((taster_name %in% ExcludedReviewers) == F, is.na(taster_name) == F) %>% 
  group_by(country) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(rank = rank(-n)) #not surprising, FR, IT, and US.  Keep these 3, group all others


#joyplot by reviewer and facet by country
wine_ratings %>% 
  filter((taster_name %in% ExcludedReviewers) == F, is.na(taster_name) == F, is.na(country) == F) %>% 
  mutate(country = factor(country),
         countrycat = fct_other(country, keep = c("US", "France", "Italy"))) %>% #first time playing with forcats, kept top 3 countries by review
  ggplot(aes(x = points, y = taster_name, fill = gender)) +
  geom_density_ridges() +
  geom_vline(xintercept = median(wine_ratings$points[(wine_ratings$taster_name %in% ExcludedReviewers & is.na(wine_ratings$country)==F)])) +
  annotate(geom = "text",label = "median", 
           x = median(wine_ratings$points[(wine_ratings$taster_name %in% ExcludedReviewers & is.na(wine_ratings$country)==F)]), 
           y = 0, 
           angle = 90,
           hjust = -.25,
           vjust = -.5) +
  labs(title = "The Joy(plot) of Wine Reviews",
       subtitle = "Excluding Reviews with No Country",
       x = "Wine Score",
       y = "Reviewer",
       caption = "@phillynerd") +
  facet_wrap(~countrycat)

#joyplot by reviewer and country
wine_ratings %>% 
  filter((taster_name %in% ExcludedReviewers) == F, is.na(taster_name) == F, is.na(country) == F) %>% 
  mutate(country = factor(country),
         countrycat = fct_other(country, keep = c("US", "France", "Italy"))) %>% #first time playing with forcats, kept top 3 countries by review
  ggplot(aes(x = points, y = taster_name, fill = countrycat)) +
  geom_density_ridges(alpha = .4) +
  geom_vline(xintercept = median(wine_ratings$points[(wine_ratings$taster_name %in% ExcludedReviewers & is.na(wine_ratings$country)==F)])) +
  annotate(geom = "text",label = "median", 
           x = median(wine_ratings$points[(wine_ratings$taster_name %in% ExcludedReviewers & is.na(wine_ratings$country)==F)]), 
           y = 0, 
           angle = 90,
           hjust = -.25,
           vjust = -.5) +
  labs(title = "The Joy(plot) of Wine Reviews",
       subtitle = "Excluding Reviews with No Country",
       x = "Wine Score",
       y = "Reviewer",
       caption = "@phillynerd") +
  scale_fill_paletteer_d(wesanderson, GrandBudapest1) #Wanted to play w/palatteer and find the most wine-like color palette

