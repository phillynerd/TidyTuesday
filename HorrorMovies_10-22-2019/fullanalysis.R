devtools::install_github("phillynerd/bloodyR", force = T)
library(bloodyR)
library(tidyverse)
library(skimr)
library(tidytext)
library(wordcloud2)
library(scales)
library(emoGG)
library(extrafont)
library(ggimage)
library(conflicted)
library(magick)

conflict_prefer("geom_emoji", "emoGG")
conflict_prefer("filter", "dplyr")

#this script uses the Nosferatu font and the Quiet Horror Story font
#both of which can be found for free online
extrafont::font_import()
extrafont::fonts()


#Raw Data
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
         release_format = case_when(movie_rating %in% c("E", "TV-14", "TV-MA", "TV-PG") ~ "TV",
                                    movie_rating %in% c("PG", "PG-13", "R") ~ "Theater",
                                    movie_rating %in% c("X", "NC-17") ~ "Adult",
                                    movie_rating %in% c("NOT RATED", "UNRATED") | is.na(movie_rating) == T ~ "Direct Release")) %>% 
  filter(MovieYear >= 2012)

#extracting genres####
#I wanted to see what was going on with genres, and how they overlapped

#max n genres = 9
horror_movies %>% 
  select(genres) %>% 
  distinct(genres) %>% 
  mutate(ngenres = str_count(genres, pattern = "\\|") + 1) %>% 
  arrange(desc(ngenres)) 

#
horror_movies %>% 
  select(genres) %>% 
 # distinct(genres) %>% 
  mutate(ngenres = str_count(genres, pattern = "\\|") + 1) %>% 
  arrange(desc(ngenres)) %>% 
  add_count(ngenres, sort = T) %>% 
  rename(NwithXnumbergenres = n) %>% #how many films have the same number of genre items listed
  filter(genres != "Horror") %>% #removing ones with only horror genre
  add_count(genres, sort = T) %>% #adding count of those w exact same genre combo
  rename(Nwithspecificgenres = n) %>% 
  distinct() #245 distinct combinations, ranging from 2 to 9 genres per film. 

#boxplot of number of films with each individual genre label by distribution type
horror_movies %>% 
  separate_rows(genres, sep = "\\|") %>% #this separates the genres into separate rows based on the pipe delimiter.  It's like splitting it all into dif columns and then turning it into a long format in one step
  filter(is.na(genres) == F) %>% 
  mutate(genres = as.factor(genres), 
         genres = fct_reorder(genres, review_rating, .fun = median, na.rm = T)) %>% #reorders things for the boxplots
  add_count(genres) %>% #adds the total N of films in each genre
  filter(n > 50)  %>% #genre has to have at least 50 films
  ggplot(aes(x = genres, y = review_rating)) +
  geom_jitter(width = .3, alpha = .3, aes(color = release_format)) +
  geom_boxplot(notch = T, outlier.shape = NA, alpha = 0) +
  coord_flip() +
  facet_wrap(~release_format) +
  scale_color_horror(palette = "NeonDemon_red") 

#wordclouds by release format, kinda fun####

words <- horror_movies %>% 
  mutate(plot = str_remove_all(plot, pattern = "(?<!w)([A-Z])\\.")) %>% #removes middle initials followed by periods
  mutate(plot = str_replace_all(plot, pattern = "Jr.", "Jr"), #fix junior period
         plot = str_replace_all(plot, pattern = "St.", "St")) %>% #fix St. period
  separate(plot, into = c("director", "cast_sentence", "plot"),
                         sep = "[//.]", 
                         extra = "merge",
                         fill = "right") %>%  #extra = "merge" merges anything beyond the first 2 sentences into the plot variable
  mutate(castsent_start = str_sub(cast_sentence, start = 1, end = 5), 
         plot_test = ifelse(castsent_start == " With", "pass", "fail")) %>% 
  filter(plot_test == "pass") %>% 
  unnest_tokens(word, plot) %>% 
  anti_join(stop_words) %>% 
  group_by(release_format, word) %>% 
  tally() %>% 
  arrange(desc(n))

D2Vwords <- words %>% 
  filter(release_format == "Direct Release",
         n > 50) %>% 
  ungroup() %>% 
  select(word, n)

Filmwords <- words %>% 
  filter(release_format == "Theater", 
         n > 5) %>% 
  ungroup() %>% 
  select(word, n)

TVwords <- words %>% 
  filter(release_format == "TV", 
         n > 3) %>% 
  ungroup() %>% 
  select(word, n)

#wordclouds by release type
wordcloud2(D2Vwords, color = rep(horror_palettes("Exorcist_Q")(10), length.out = nrow(D2Vwords)), size = .4, backgroundColor = "black")
wordcloud2(Filmwords, color = rep(horror_palettes("Suspiria")(10), length.out = nrow(Filmwords)), size = .4, backgroundColor = "grey") 
wordcloud2(TVwords, color = rep(horror_palettes("NeonDemon_blue")(10), length.out = nrow(TVwords)), size = .5, backgroundColor = "grey20") 


#More text analysis####
#comparing top 10 words for each genre
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
                                     size = 18, hjust = .5,
                                     color = horror_colors("vintageblood")),
        plot.caption = element_text(color = "white", family = "Quiet Horror Story",
                                    size = 20),
        strip.text = element_blank()) +
  labs(title = "The Last Word On The Left",
       subtitle = "Top 10 most prevalent words by distribution",
       y = "Word Prevalence\n(n appearances/n plots)",
       caption= "Data: IMDb | Viz: @phillynerd")

#adding in background
house <- "http://s1.1zoom.net/big0/739/349023-admin.jpg"

ggbackground(plot1, house) %>% 
  ggsave(filename = "LWOTL.png", device = "png", height = 5, width = 7, dpi = 300, units = "in")
  
img_raw <-
  image_read("zombie.png") %>% 
  image_scale("300")

plot <- image_read("LWOTL.png")

image_composite(plot, img_raw, operator = "atop", offset = "+200+1250") %>% 
  image_write("hauntedhouseplot.png")


