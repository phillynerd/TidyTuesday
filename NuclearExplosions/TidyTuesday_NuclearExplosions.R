#Data Dictionary###

#variable	class	description
#date_long	date	ymd date
#year	double	year of explosion
#id_no	double	unique ID
#country	character	Country deploying the nuclear device
#region	character	Region where nuclear device was deployed
#source	character	Source the reported the explosion event
#latitude	double	Latitude position
#longitude	double	Longitude position
#magnitude_body	double	Body wave magnitude of explosion (mb)
#magnitude_surface	double	Surface wave magnitude of explosion (Ms)
#depth	double	Depth at detonation in Km (could be underground or above ground) -- please note that positive = depth (below ground), while negative = height (above ground)
#yield_lower	double	Explosion yield lower estimate in kilotons of TNT
#yield_upper	double	Explosion yield upper estimate in kilotons of TNT
#purpose	character	Purpose of detonation: COMBAT (WWII bombs dropped over Japan), FMS (Soviet test, study phenomenon of nuclear explosion), ME (Military Exercise), PNE (Peaceful nuclear explosion), SAM (Soviet test, accidental mode/emergency), SSE (French/US tests - testing safety of nuclear weapons in case of accident), TRANSP (Transportation-storage purposes), WE (British, French, US, evaluate effects of nuclear detonation on various targets), WR (Weapons development program)
#name	character	Name of event or bomb
#type	character	type - method of deployment -- ATMOSPH (Atmospheric), UG (underground), BALLOON (Balloon drop), AIRDROP (Airplane deployed), ROCKET (Rocket deployed), TOWER (deplyed at top of constructed tower), WATERSURFACE (on surface of body of water), BARGE (on barge boat), SURFACE (on surface or in shallow crater), UW (Underwater), SHAFT (Vertical Shaft underground), TUNNEL/GALLERY (Horizontal tunnel)

#libraries
install.packages("extrafont")
library(tidyverse)
library(skimr)
library(visdat)
library(tidylog)
library(lubridate)

library(extrafont)
extrafont::font_import()
loadfonts(device = "win")
fonts()


#What i'd use if i wasn't on my lame work computer with all it's lame firewalls
#nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

#what I have to use instead:
NE_Raw <- read_csv("RawData/nuclear_explosions.csv") %>% #had to copy and paste data from git, then need to reclasify so it matches the git version
  mutate(date_long = ymd(date_long),
         country = factor(country),
         region = factor(region),
         source = factor(source),
         type = factor(type)) %>% 
  select(-X17)

skim(NE_Raw)
vis_dat(NE_Raw)
vis_miss(NE_Raw)

#creating 2 master categories for type, basd on documentation
NE_Raw %>% 
  count(type) %>% 
  arrange(desc(n)) 

atm <- c("TOWER", "AIRDROP", "UW", "SURFACE", "CRATER", "SHIP", 
         "ATMOSPH", "BALLOON", "ROCKET", "WATERSUR", 
         "WATER SU", "BARGE")

ug <- c("SHAFT", "MINE", "TUNNEL", "GALLERY", "UG", "SHAFT/GR", "SHAFT/LG")

space <- "SPACE"

#looking at countries
NE_Raw %>% 
  count(country)

NEclean <- NE_Raw %>% 
  mutate(typecat = case_when(type %in% atm ~ "Atmospheric",
                             type %in% ug ~ "Underground",
                             type %in% space ~ "Space")) %>%
  filter(is.na(yield_lower) == F, is.na(yield_upper)==F) %>% 
  mutate(AverageYield = (yield_lower + yield_upper)/2)

skim(NEclean)

NEclean %>% 
  ggplot(aes(x = AverageYield)) +
  geom_density()

#trying some viz
basefig <- NEclean %>% 
  ggplot(aes(x = country, y = year, size = AverageYield, color = typecat)) +
  annotate(geom = "rect", #peak nuclear explosions by megatonage (wikipedia:https://en.wikipedia.org/wiki/Nuclear_weapons_testing#History)
           xmin = -Inf, 
           xmax = Inf, 
           ymin = 1961, 
           ymax = 1962, 
           fill = "#03525E", 
           alpha = .8) +
  annotate(geom = "rect", #USSR-US bilateral testing moratorium on testing 
           xmin = -Inf, 
           xmax = Inf, 
           ymin = 1958, 
           ymax = 1961, 
           fill = "#09c2de", 
           alpha = .6) +
  geom_hline(yintercept = 1963, color = "#ffbb00", size = 1) + #Partial nuclear test ban treaty in effect, oct 10, 1963
  geom_hline(yintercept = 1967, color =  "#00ff44", size = 1) + #outer space treaty
  geom_jitter(alpha = .4, width = .2) +
  scale_y_continuous(breaks = seq(min(NEclean$year), 
                                  max(NEclean$year),  
                                  5)) +
  scale_color_manual(values = c("#9e0c02", "#00ff44","#ffdd00")) +
  scale_x_discrete(labels = c("PAKIST" = "Pakistan", 
                              "INDIA" = "India", 
                              "FRANCE" = "France", 
                              "CHINA" = "China")) 
annotatefig <- basefig +
  coord_flip() +
  #note and arrow for moratorium
  annotate(geom = "text", x = 2, y = 1952, 
           label = "USSR & US agree to\nbilateral testing moratorium",
           size = 3.5, color = "white", family = "Gill Sans MT") +
  geom_curve(aes(y = 1952, yend = 1959, x = 1.7, xend = 1.7), 
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "white", curvature = 0.25) +
  # note and arrow for peak testing period
  annotate(geom = "text", x = 4, y = 1957, 
           label = "Period represents peak \ntesting by megatonage",
           size = 3.5, color = "white", family = "Gill Sans MT") +
  geom_curve(aes(y = 1957, yend = 1961.5, x = 3.7, xend = 3.7), 
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "white", curvature = 0.25) +
  # note for underground testing only
  annotate(geom = "text", x = 5.5, y = 1972, 
           label = "Partial Nuclear Test Ban Treaty\nlimits all testing to underground",
           size = 3.5, color = "white", family = "Gill Sans MT") +
  geom_curve(aes(y = 1972, yend = 1963, x = 5.2, xend = 5.2), 
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "white", curvature = -0.25) +
  # note for underground testing only
  annotate(geom = "text", x = 3, y = 1974, 
           label = "Outer Space Treaty bans all testing\non the moon and other celestial bodies",
           size = 3.5, color = "white", family = "Gill Sans MT") +
  geom_curve(aes(y = 1974, yend = 1967, x = 2.7, xend = 2.7), 
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "white", curvature = -0.25) +
  # Hiroshima and nagasaki
  annotate(geom = "text", x = 5, y = 1945, 
           label = "Hiroshima &\nNagasaki",
           size = 3.5, color = "white", family = "Gill Sans MT") +
  geom_curve(aes(y = 1944.2, yend = 1944.6, x = 5.2, xend = 5.9), 
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "white", curvature = -0.25) +
  labs(title = "The Atomic Bomb and You",
        subtitle = sprintf( "A Timeline of Nuclear Testing and Select Treaties: %s - %s", 
                       min(NEclean$year), 
                       max(NEclean$year)),
       caption = "Data: SIPRI | Treaty Information: Wikipedia | Visualization: @phillynerd",
       size = "Average Yield\n(Kilotons of TNT)",
       color = "Testing Location") 

finalfig <- annotatefig +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray20"),
        panel.border = element_rect(color = "white", fill = NA),
        plot.background  = element_rect(fill = "gray20"),
        legend.background = element_rect(fill = "gray20"),
        legend.text = element_text(color = "white", size = 10, family = "Epyval"),
        legend.title = element_text(color = "#ffbf00", size = 14),
        legend.position = "bottom",
        legend.key = element_rect(fill = NA, color = NA), #removes little boxes behind legend widget
        axis.text = element_text(color = "white", family = "Epyval", size = 12),
        title = element_text(color = "white", family = "Epyval"),
        plot.title = element_text(size = 37, face = "italic", color = "#ffbf00"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 13),
        axis.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 6, alpha = 1)),
         size = guide_legend(override.aes = list(color = "white"))) #changes size of color legend, and sets alpha to 1



