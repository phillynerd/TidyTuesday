#Background####
#Tidy Tuesday, April 2019, datasource: seattle.gov
#companion article: https://www.seattletimes.com/seattle-news/transportation/what-we-can-learn-from-seattles-bike-counter-data/

#Library####
library(tidyverse)
library(skimr)
library(visdat)
library(lubridate)
library(tidylog)
library(ggpubr)



#reading in data####

bike_traffic <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv",
                         col_types = "cffdd")

#cleaning and recasting####
skim(bike_traffic)

vis_dat(bike_traffic, warn_large_data = F)

head(bike_traffic)


#most reliable meters: "Counters at the Fremont Bridge, Elliott Bay Trail, Burke-Gilman Trail, Second Avenue and the Spokane Street Bridge are the most reliable.

bike_traffic$date <- mdy_hms(bike_traffic$date)
bike_traffic$date2 <- as.Date(bike_traffic$date)
#exploring####

    #total volume by month and direction for each counter####
bike_traffic %>%
  filter(is.na(bike_count) == FALSE) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(month, crossing, direction) %>% 
  summarize(n = sum(bike_count)) %>% 
  ggplot(aes(x = month, y = n, color = direction)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1:12)) +
  facet_wrap(~crossing)

#avg volume by month and direction for each counter####
bike_traffic %>%
  filter(is.na(bike_count) == FALSE) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(month, crossing, direction) %>% 
  summarize(avgmonthlybike = mean(bike_count, na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = avgmonthlybike, color = direction)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1:12)) +
  facet_wrap(~crossing)

    #total volume over time####
bike_traffic %>%
  filter(is.na(bike_count) == FALSE) %>% 
  mutate(YM = zoo::as.yearmon(date),
         year = year(date)) %>% 
  group_by(YM, year) %>% 
  summarize(n = sum(bike_count)) %>% 
  ggplot(aes(x = YM, y = n)) +
  geom_line() +
  geom_smooth(method = 'lm') 
  
    #total volume over time, faceted by crossing####
bike_traffic %>%
  filter(is.na(bike_count) == FALSE, crossing != "Sealth Trail") %>% 
  mutate(YM = zoo::as.yearmon(date),
         year = year(date)) %>% 
  group_by(YM, year, crossing) %>% 
  summarize(n = sum(bike_count)) %>% 
  ggplot(aes(x = YM, y = n)) +
  geom_line() +
  geom_smooth(method = 'lm') +
  facet_wrap(~crossing)

#Bringing in Weather Data####
W1 <- read.csv("NOAA Weatherfile 1.csv")

W2 <- read.csv("NOAA Weatherfile 2.csv")

#creating weather database####
Weather <- W1 %>% 
  bind_rows(W2) %>% 
  filter(STATION == "USW00024233") %>% #only station with avg temp data
  select(STATION, DATE, TAVG, TMIN, TMAX, PRCP) %>% 
  mutate(DATE = ymd(DATE), 
         month = month(DATE),
         season = ifelse(month %in% c(3, 4, 5), "Spring",
                        ifelse(month %in% c(6,7,8), "Summer", 
                               ifelse(month %in% c(9, 10, 11), "Fall", "Winter"))))

#weather over time####
Weather %>% 
  ggplot(aes(x = DATE, y = TAVG)) +
  geom_line() +
  geom_smooth() +
  scale_x_date (date_breaks = "3 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#precip over time, with seasonality####
Weather %>% 
  ggplot(aes(x = DATE, y = PRCP)) +
  geom_line()+
  geom_point(aes(color = season), alpha = .5) +
  scale_x_date (date_breaks = "3 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#merging weather data with bike data

bike_traffic %>% 
  filter(is.na(bike_count) == FALSE) %>% 
  group_by(date2) %>% #figure out how to strip time off of date, group by day w total bike-count, then add in weather
  summarize(DailyBikeCount = sum(bike_count)) %>%
  ggplot(aes(x = date2, y = DailyBikeCount)) +
  geom_line()
  
bike_traffic %>% 
  filter(date2 == as.Date("5/30/2018", format = "%m/%d/%Y")) %>%  View() #exploring the outlier on 5/30/2018

#checking out weird outlier
bike_traffic %>% 
  mutate(time = as.POSIXct(strftime(date, format = '%H:%M:%S', usetz = TRUE, tz = "GMT"),format = "%H:%M:%S")) %>% #ggplot only uses positct for axes
  filter(date2 == as.Date("5/30/2018", format = "%m/%d/%Y")) %>% 
  group_by(crossing,  time) %>% 
  summarize(totaln = sum(bike_count)) %>% 
  ggplot(aes(x = time, y = totaln)) +
  geom_line(stat = 'identity') +
  facet_wrap(~crossing) #WTF> Delete burke gilman trail on 5/30/2019

#removing outlier and looking at daily trends.
bike_traffic %>% 
  filter(is.na(bike_count) == FALSE, 
         !(crossing == "Burke Gilman Trail" & date2 == as.Date("5/30/2018", format = "%m/%d/%Y"))) %>% 
  group_by(date2) %>% #figure out how to strip time off of date, group by day w total bike-count, then add in weather
  summarize(DailyBikeCount = sum(bike_count)) %>%
  ggplot(aes(x = date2, y = DailyBikeCount)) +
  geom_line()

#removing outlier and adding in weather data for BW dataset
BnW <- bike_traffic %>% 
  filter(is.na(bike_count) == FALSE, 
         !(crossing == "Burke Gilman Trail" & date2 == as.Date("5/30/2018", format = "%m/%d/%Y"))) %>% 
  group_by(date2) %>% #figure out how to strip time off of date, group by day w total bike-count, then add in weather
  summarize(DailyBikeCount = sum(bike_count, na.rm = TRUE)) %>% 
  left_join(Weather, by = c("date2" = "DATE")) %>% 
  mutate(season = as_factor(season)) %>% 
  mutate(season = fct_relevel(season, "Spring", "Summer", "Fall", "Winter"))

#bik traffic by temp
temp <- BnW %>% 
  ggplot(aes(x = DailyBikeCount, y = TAVG, color = season)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  facet_wrap(~season) +
  labs(title = "By Average Daily Temperature",
     #  subtitle = paste0("Seattle: ", min(BnW$date2), " to ", max(BnW$date2)),
       x = "Daily Bike Count",
       y = "Avg Daily Temp (F)")+
  theme(legend.position = "")

#precipitation
precip <- BnW %>% 
  ggplot(aes(x = DailyBikeCount, y = PRCP, color = season)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  facet_wrap(~season) +
  labs(title = "By Total Daily Precipitation",
      # subtitle = paste0("Seattle: ", min(BnW$date2), " to ", max(BnW$date2)),
       x = "Daily Bike Count",
       y = "Total Daily Precipitation (in)") +
  theme(legend.position = "")


BnW %>% 
  ggplot(aes(x = PRCP, y = TAVG, color = DailyBikeCount)) +
  geom_jitter(alpha = .2, size = 3) +
  geom_smooth(se = F) +
#  scale_x_log10() +
  scale_color_viridis_c(option = "B") +
  facet_wrap(~season)



both <- BnW %>% 
  ggplot(aes(x = DailyBikeCount, y = TAVG, color = PRCP)) +
  geom_jitter(alpha = .1, size = 2) +
  geom_smooth(se = F) +
  scale_color_viridis_c(option = "B") +
  facet_wrap(~season) +
  labs(title = "Average Daily Temperature and Total Daily Precipitation",
       x = "Daily Bike Count",
       y = "Avg Daily Temperature (F)",
       color = "Total Precipitation (in)")

#final figure####
annotate_figure(ggarrange(temp, precip, both,
                          nrow = 2, ncol = 2),
                top = text_grob(paste0("Seasonal Variations in Daily Bike Ridership, Seattle: ",
                                       min(BnW$date2), " to ", max(BnW$date2)),
                                face = "bold", size = 14),
                bottom = text_grob("Data source: bike data - seattle.gov; weather data - NOAA|@phillynerd", 
                                             hjust = 1.01, x = 1, face = "italic", size = 10))



