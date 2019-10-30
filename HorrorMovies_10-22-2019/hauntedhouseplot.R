#Overview####
#This is just a short script for generating the full tidytuesday submission.
#The sole purpose is for ease of use
#Full analysis, including this code, available in other script

devtools::install_github("phillynerd/bloodyR", force = T)
library(bloodyR)
library(tidyverse)
library(tidytext)
library(scales)
library(emoGG)
library(extrafont)
library(ggimage)
library(magick)
library(conflicted)

conflict_prefer("geom_emoji", "emoGG")
conflict_prefer("filter", "dplyr")

#this script uses the Nosferatu font and the Quiet Horror Story font
#both of which can be found for free online. 
extrafont::font_import()
extrafont::fonts()


#Raw Data####
horror_movies_orig <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

#basic cleaning####
horror_movies <- horror_movies_orig %>% 
  distinct() %>% #removing duplicates
  mutate(language = as.factor(language),
         release_country = as.factor(release_country),
         movie_rating = as.factor(movie_rating),
         movie_run_time = as.numeric(str_remove(movie_run_time," min")),
         MovieYear = as.numeric(str_sub(title, start = -5, end = -2)),#only fails in 1 case
         budget = parse_number(budget), #fun little function that pulls out just the number
         #assuming unrated films are likely direct to video or direct to streaming releases
         release_format = case_when(movie_rating %in% c("E", "TV-14", "TV-MA", "TV-PG") ~ "TV",
                                    movie_rating %in% c("PG", "PG-13", "R") ~ "Theater",
                                    movie_rating %in% c("X", "NC-17") ~ "Adult",
                                    movie_rating %in% c("NOT RATED", "UNRATED") | is.na(movie_rating) == T ~ "Direct Release")) %>% 
  filter(MovieYear >= 2012)

#Using tidytext to get words from plot

words <- horror_movies %>% 
  mutate(plot = str_remove_all(plot, pattern = "(?<!w)([A-Z])\\.")) %>% #removes middle initials followed by periods so my plot split works
  mutate(plot = str_replace_all(plot, pattern = "Jr.", "Jr"), #fix Jr. period
         plot = str_replace_all(plot, pattern = "St.", "St")) %>% #fix St. period
  separate(plot, into = c("director", "cast_sentence", "plot"),
           sep = "[//.]", 
           extra = "merge", #extra = "merge" merges anything beyond the first 2 sentences into the plot variable
           fill = "right") %>%  
  mutate(castsent_start = str_sub(cast_sentence, start = 1, end = 5), #If this parsing worked correctly, then the cast_sentence should always start with " With"
         plot_test = ifelse(castsent_start == " With", "pass", "fail")) %>% 
  filter(plot_test == "pass") %>% #Removes cases that still aren't parsing as expected; not many
  unnest_tokens(word, plot) %>% #pulls out words from the movie plots
  anti_join(stop_words) %>% #removes stop words
  group_by(release_format, word) %>% 
  tally() %>% 
  arrange(desc(n))

#comparing top 10 words for each genre
#total number of distinct titles in the dataset
horror_title_count <- horror_movies %>% 
  distinct(title, .keep_all = T) %>% 
  count(release_format) %>% 
  rename(denom = n)

#just not the most interesting plot to work with
words %>% 
  top_n(10) %>% 
  left_join(horror_title_count, by= "release_format") %>% 
  mutate(wordprevalence = n/denom) %>% 
  ggplot(aes(x = reorder(word, wordprevalence), y = wordprevalence, fill = release_format)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~release_format) +
  scale_fill_horror() +
  scale_y_continuous(label = percent)

#but what if we add spiders????
oospiders <- words %>% 
  top_n(10) %>% 
  left_join(horror_title_count, by= "release_format") %>% 
  mutate(wordprevalence = n/denom) %>% 
  ggplot(aes(x = reorder(word, -wordprevalence), y = wordprevalence)) +
  geom_emoji(emoji = "1f577") +
  geom_segment(aes(xend = word, 
                   y = 0, yend = wordprevalence, 
                   color = release_format), 
               size = .8) +
  facet_wrap(~release_format) +
  scale_color_horror() +
  scale_x_discrete(position = "top") +
  scale_y_reverse(label = percent, limits = c(.2, 0)) 

#adding theme elements
plot1 <- oospiders +
  geom_hline(yintercept = 0, color = "grey60", size = .4) +
  ggthemes::theme_tufte() +
  theme(legend.position = "top",
        legend.text = element_text(family = "Nosferatu",
                                   size = 14),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(family = "Nosferatu",
                                 size = 10, color = "white"),
        axis.title = element_text(family = "Nosferatu",
                                  size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(family = "Quiet Horror Story", 
                                  size = 65, hjust = .5,
                                  color = horror_colors("brightblood")),
        plot.subtitle = element_text(family = "Nosferatu", 
                                     size = 16, hjust = .5,
                                     color = horror_colors("vintageblood")),
        plot.caption = element_text(color = "white", family = "Quiet Horror Story",
                                    size = 20),
        strip.text = element_blank()) +
  labs(title = "The Last Word On The Left",
       subtitle = "Top 10 most prevalent plot words by distribution type",
       y = "Word Prevalence\n(n appearances/n plots)",
       caption= "Data: IMDb | Viz: @phillynerd")

#adding in background
house <- "http://s1.1zoom.net/big0/739/349023-admin.jpg"

#making composite image
ggbackground(plot1, house) %>% 
  ggsave(filename = "LWOTL.png", device = "png", height = 5, width = 7, dpi = 300, units = "in")

#adding a zombie hand in the bottom bc everything is better with zombies
img_raw <-
  image_read("zombie.png") %>% 
  image_scale("300")

plot <- image_read("LWOTL.png")

image_composite(plot, img_raw, operator = "atop", offset = "+200+1250") %>% 
  image_write("hauntedhouseplot.png")



