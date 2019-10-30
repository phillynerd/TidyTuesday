#squirrel tidy tuesday
#just a quick silly map to see where squirrels make sounds, and play with map visualizations a bit

#libraries####
library(tidyverse)
library(skimr)
library(leaflet)
library(htmlwidgets)

nyc_squirrels_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

#some quick recasting
squirrels <- nyc_squirrels_raw %>% 
  mutate(community_districts = as.factor(community_districts),
         zip_codes = as.factor(zip_codes),
         primary_fur_color = as.factor(primary_fur_color),
         age = as.factor(age))

#creating a palette based on certain responses
#interesting, hadn't seen this function before
pal <- colorFactor(c("green", "blue", "red", "grey"), domain = c("quaas", "kuks", "moans", "silent"))

#going to long format and creating map
squirrelmap <- squirrels %>% 
  mutate(silent = ifelse(quaas + kuks + moans == 0, 1, 0)) %>% 
  pivot_longer(cols = c(quaas, kuks, moans, silent), names_to = "Sounds", values_to = "YN_Sound" ) %>% 
  filter(YN_Sound == "1") %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% #nice minimalist tiles
  addCircleMarkers(~long, ~lat, 
                   radius = 3, 
                   color = ~pal(Sounds),
                   opacity = .3) %>% #
  addLegend(pal = pal, values = ~Sounds, position = "bottomleft")

saveWidget(squirrelmap, file="squirrelmap.html")
