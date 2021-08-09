#Overview ####
#Tidy Tuesday Overview!!

#installing packages if needed
#install.packages("tidytuesdayR")
#install.packages("tidyverse)
#install.packages("lubridate")
#install.packages("scales")
#install.packages("ggthemes")

#libraries
library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)

#Reading in the data using the tidy tuesday package

tuesdata <- tidytuesdayR::tt_load('2021-06-29')
#if you open tuesdat, you can see all the readme info on this dataset.
#Includes a data dictionary!


#but how do we get the data?
str(tuesdata)
#tuesdat is actuallky a list object.  Looks like the data is in the first item.
#Don't worry, you don't need to know this, tht code bit is usually on the github
#repo for you


animal_rescues_raw <- tuesdata$animal_rescues

#So let's do some really basic exploration.  When is this data from?
str(animal_rescues_raw$date_time_of_call)
#so this is a character currently. which means i need to convert it to a date.

animal_rescues_clean <- animal_rescues_raw %>%
  mutate(date_time_of_call = lubridate::dmy_hm(date_time_of_call))

animal_rescues_clean %>%
  summarise(min(date_time_of_call),
            max(date_time_of_call))

#Lets do a count by years
animal_rescues_clean %>%
  count(cal_year)

#lets plot this to see this more clearly.
animal_rescues_clean %>%
  #instead of just running a quick and dirty count, lets actually make a year variable
  count(cal_year) %>%
  ggplot(aes(x = cal_year, y = n)) +
  geom_bar(stat = "identity")

#any seasonality if i look at the raw dates over year?
animal_rescues_clean %>%
  mutate(call_date = as.Date(date_time_of_call)) %>%
  count(call_date, cal_year) %>%
  ggplot(aes(x = month(call_date),
             y = n,
             color = factor(year(call_date)))) +
 # geom_line() +
  geom_smooth() +
  #facet_wrap(~cal_year)
  scale_x_continuous(breaks = seq(1:12), labels = month.abb)

#ok what kinds of animals are in this dataset
animal_rescues_clean %>%
  count(animal_group_parent) %>%
  #ordering by the $ value with reorder()
  ggplot(aes(x = reorder(animal_group_parent, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

#what animals are the most expensive to deal with?
animal_rescues_clean %>%
  #cleanup - making animal names all caps; changing incidental costs from chr
  # to numeric so we can add
  mutate(animal_group_parent = str_to_title(animal_group_parent),
         incident_notional_cost = as.numeric(incident_notional_cost)) %>%
  #grouping
  group_by(animal_group_parent) %>%
  #creating sums (sum) and counts (n()) over the grouped variable
  summarize(n_events = n(),
            total_cost = sum(incident_notional_cost, na.rm = T)) %>%
  #calculating the avg
  mutate(avg_cost_per_event = total_cost/n_events) %>%
  ggplot(aes(x = reorder(animal_group_parent, avg_cost_per_event), y = avg_cost_per_event)) +
  geom_bar(stat = "identity") +
  coord_flip()

#how about some boxplots on this?
animal_rescues_clean %>%
  #cleanup
  mutate(animal_group_parent = str_to_title(animal_group_parent),
         incident_notional_cost = as.numeric(incident_notional_cost)) %>%
  ggplot(aes(x = incident_notional_cost,
             #order the animals by median, from high to low
             y = fct_reorder(animal_group_parent,
                             incident_notional_cost,
                             .fun = median, na.rm = T),
             color = animal_group_parent)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.3, alpha = 0.5) +
  theme(legend.position = "blank")

#ok lets run with this.  Let's remove animals w < 100 cases.
data1 <- animal_rescues_clean %>%
  #cleanup
  mutate(animal_group_parent = str_to_title(animal_group_parent),
         incident_notional_cost = as.numeric(incident_notional_cost)) %>%
  #adding count per group
  group_by(animal_group_parent) %>%
  add_count(name = "total_events_per_animal") %>%
  filter(total_events_per_animal >=100)

(plot1 <- data1 %>%
  ggplot(aes(x = incident_notional_cost,
             #order the animals by median, from high to low
             y = fct_reorder(animal_group_parent,
                             incident_notional_cost,
                             .fun = median, na.rm = T),
             color = animal_group_parent)) +
  geom_jitter(width = 0.3, alpha = 0.5) +
  geom_boxplot(alpha = 0.5, color = "black", outlier.shape = NA, notch = T) #+
  #theme(legend.position = "blank")
)

#let's pretty this up a bit
(plot2 <- plot1 +
  scale_x_continuous(labels = dollar) +
  coord_cartesian(xlim = c(0,1500)) +
  ggthemes::theme_wsj() +
  theme(legend.position = "blank") +
  labs(title = "Hold Your Horses!",
       subtitle = "Animal Rescues by Cost",
       x = "Total Cost Per Incident",
       y = "Species",
       caption = "Source: London.gov; Image: @phillynerd")
)

#and one more for fun
data1 %>%
  ggplot(aes(x = incident_notional_cost,
                     #order the animals by median, from high to low
                     y = fct_reorder(animal_group_parent,
                                     incident_notional_cost,
                                     .fun = median, na.rm = T),
                     color = animal_group_parent)) +
  geom_jitter(width = 0.3, alpha = 0.5) +
  geom_boxplot(alpha = 0.5, color = "black", outlier.shape = NA) +
  facet_wrap(~cal_year) +
  scale_x_continuous(labels = dollar) +
  coord_cartesian(xlim = c(0,1200)) +
  ggthemes::theme_wsj() +
  theme(legend.position = "blank",
        axis.text.x = element_text(size = 10,
                                   angle = 90)) +
  labs(title = "Hold Your Horses!",
       subtitle = "Animal Rescues by Year and Cost",
       x = "Total Cost Per Incident",
       y = "Species",
       caption = "Source: London.gov; Image: @phillynerd")

