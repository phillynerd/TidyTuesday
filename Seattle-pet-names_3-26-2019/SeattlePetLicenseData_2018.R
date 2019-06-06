#Overview####
#Tidytuesday task - visualizing pet reg

#packages####
install.packages('gender')
library(gender)
library(tidyverse)
library(skimr)
library(visdat)
library(magrittr)

#reading files####
pets <- read_csv("Seattle_Pet_Licenses.csv")

#cleaning data####
skim(pets)
vis_miss(pets)



pets %<>%
  mutate(`License Issue Date` = as.Date(pets$`License Issue Date`, format = '%B %d %Y'),
         `ZIP Code` = str_trunc(`ZIP Code`, 5, ellipsis = ""),
         Species = as.factor(Species)) %>%
  rename(LicenseIssueDate = `License Issue Date`,
         LicenseNo = `License Number`,
         PetName = `Animal's Name`,
         PrimaryBreed = `Primary Breed`,
         SecondaryBreed = `Secondary Breed`,
         Zip = `ZIP Code`)

#What does data look like when Pet Names are missing
pets %>%
  filter(is.na(PetName)) %>%
  group_by(Zip) %>%
  tally() %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(Zip, n), y = n)) +
  geom_bar(stat = 'identity') +
  coord_flip()

#What counties have the highest volume of missing names
denomZip <- pets %>%
  group_by(Zip) %>%
  tally() %>%
  arrange(desc(n)) %>%
  rename(denom = n)

pets %>%
  filter(is.na(PetName)) %>%
  group_by(Zip) %>%
  tally() %>%
  arrange(desc(n)) %>%
  left_join(denomZip, by = 'Zip') %>%
  mutate(percent = round(n/denom, 3)) %>%
  ggplot(aes(x = reorder(Zip, percent), y = percent)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Seattle Zip Codes with the Highest Proportion of Unnamed Pets",
       subtitle = "2018 Seattle Pet License Data, data.seattle.gov") +
  ylab("Percent") +
  xlab("Zip Code") +
  coord_flip() 

#Missing by species
denomSpecies <- pets %>%
  group_by(Species) %>%
  tally() %>%
  arrange(desc(n)) %>%
  rename(denom = n)

pets %>%
  filter(is.na(PetName)) %>%
  group_by(Species) %>%
  tally() %>%
  arrange(desc(n)) %>%
  left_join(denomSpecies, by = 'Species') %>%
  mutate(percent = round(n/denom, 3)) %>%
  ggplot(aes(x = reorder(Species, percent), y = percent, fill = Species)) + #can get counts, no clue how to get percents though
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Species with the Highest Proportion of Unnamed Pets",
       subtitle = "2018 Seattle Pet License Data, data.seattle.gov") +
  ylab("Percent of Licenses with Missing Names") +
  xlab("Species") +
  viridis::scale_fill_viridis(option = "plasma", discrete = TRUE, direction = -1) +
  coord_flip() 


#Figuring out pet gender####
#Tried distinguishing between ambiguous human names and "genderless" pet names with no human equivalent.  
#Most ambiguous names are pet specific
  
GenderingNames <- gender(names = pets$PetName, method = "ssa") %>% #didn't specify years to cast widest net possible
  mutate(GenderCat = ifelse(proportion_female >= .6, "female",
                            ifelse(proportion_female <= .4, "male", "ambiguous"))) %>% 
  select(name, proportion_female, GenderCat) %>%
  distinct()

pets %>%
  filter(Species %in% c("Cat", "Dog"), is.na(PetName) == FALSE) %>%
  left_join(GenderingNames, by = c("PetName" = "name")) %>%
  mutate(GenderCat = as.factor(ifelse(is.na(GenderCat), "ambiguous", GenderCat))) %>%
  ggplot(aes(x = Species, fill = GenderCat)) +
  geom_bar(stat = 'count', position = "fill") +
  geom_text(stat = 'count', aes(label = ..count..), position = position_fill(vjust = .5), color = 'white') +
  labs(title = "How Gendered are Pet Names?", 
       subtitle = "2018 Seattle Pet License Data, data.seattle.gov",
       fill = "Gender Category") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c("purple", "#ff33cc", "blue"))
  
#checking results of stacked bar by doing the roll-ups myself, then using denomSpecies
  
denomSpecies2 <- pets %>%
  filter(Species %in% c("Cat", "Dog"), is.na(PetName) == FALSE) %>%
  group_by(Species) %>%
  tally() %>%
  arrange(desc(n)) %>%
  rename(denom = n)
  
pets %>%
  filter(Species %in% c("Cat", "Dog"), is.na(PetName) == FALSE) %>%
  left_join(GenderingNames, by = c("PetName" = "name")) %>%
  mutate(GenderCat = as.factor(ifelse(is.na(GenderCat), "ambiguous", GenderCat))) %>%
  group_by(GenderCat, Species) %>%
  tally() %>%
  left_join(denomSpecies2, by = "Species") %>%
  mutate(percent = n/denom) %>%
  ggplot(aes(x = Species, y = percent, fill = GenderCat )) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(percent)), position = position_fill(vjust = .5), color = 'white') +
  labs(title = "How Gendered are Pet Names?", 
     subtitle = "2018 Seattle Pet License Data, data.seattle.gov",
     fill = "Gender Category") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c("purple", "#ff33cc", "blue"))

#how common are our dog names####
pets %>%
  filter(Species == "Dog", is.na(PetName) == FALSE) %>%
  mutate(PetName = str_to_lower(PetName)) %>%
  group_by(PetName) %>%
  tally() %>%
  arrange(desc(n)) %>%
  mutate(rank = round(rank(-n),0)) %>%
  filter(PetName %in% c("rufio", "stanley", 'boss', 'tweak')) %>%
  kableExtra::kable(caption = 'How Common Are Our Dog Names') %>%
  kableExtra::kable_styling(full_width = FALSE, 
                            position = "center", 
                            bootstrap_options = 'striped')

#Most frequent breed for stanley: lab, rufio: terrier, boss: 4 way tie





