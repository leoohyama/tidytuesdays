##week 49 tidy tuesday script
library(tidyverse)
library(viridis)
library(wesanderson)
#read in data
shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

colnames(shelters)
sort(unique(shelters$shelter_city))

#let's make some new variables like occupancy percentage
ranks<-shelters %>% 
  group_by(program_name) %>% 
  summarise(total=n()) %>% 
  arrange(desc(total)) %>%
  filter(total > 1000)

#clean up data
shelters<-shelters %>% 
  mutate(occ_perc = occupancy/capacity, remaining_space = capacity-occupancy) %>% 
  filter(!occupancy == 0) %>%
  filter(program_name %in% ranks$program_name)

#get new column just for year
shelters$year<-as.numeric(substring(shelters$occupancy_date,1,4)) 

#work with the data + plotting

shelters %>% mutate(month = format(occupancy_date, "%m%yY")) %>%
  filter(shelter_city == "Toronto") %>%
  group_by(month, sector) %>%
  summarise(remaining_spaces = mean(remaining_space)) %>%
  ggplot(.) + geom_col(aes(x = month, y = remaining_spaces, color = sector,fill =sector)) +
  scale_fill_manual(values = wes_palette("Zissou1")) +
  scale_color_manual(values = wes_palette("Zissou1")) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "white", size=1) +
  labs(y = "Remaining space in shelter", x = "Time binned by months", 
       title = "Average remaining space in a Toronto Shelter across 2017 to 2020", color = "Shelter type",
       fill = "Shelter type") +
  theme(legend.position = "right",
        plot.background =  element_rect(fill = "gray25"),
        legend.background = element_rect(fill = "gray25"),
        panel.background = element_rect(fill = "black", color = "grey"),
        legend.text = element_text(size = 14, color = "white", family="Comic Sans MS"),
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "white", size = 14),
        axis.title = element_text(color = "white", size= 16, family="Comic Sans MS"),
        title = element_text(color = "white",size = 16,family="Helvetica"))



