#Load data and packages
library(tidyverse)
library(viridis)
hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

head(hike_data)

#fix up data formats
hike_data$rating<-as.numeric(hike_data$rating) 
hike_data$gain<-as.numeric(hike_data$gain)
hike_data$highpoint<-as.numeric(hike_data$highpoint)

#string pattern recognition to consolidate data into larger categories
#essentially grouping things by geographical areas of Washington state
hike_data$length<-as.numeric(gsub("([0-9]+).*$", "\\1", hike_data$length))
hike_data$wide_location<-gsub("\\-.*", "", hike_data$location) #get the locality before dashes
hike_data$wide_location<-sub("\\s+$", "", hike_data$wide_location) #get rid of trailing white spaces in the locality words
sort((unique(hike_data$wide_location))) #double check

###plot the data, let's do a bar plot showing both average length of the trails based on geographic area
# also the avergae alititude gaine and the average rating
hike_data %>% 
  group_by(wide_location) %>%
  summarise(mean_rating = mean(rating), mean_length = mean(length), mean_gain = mean(gain)) %>%
  ggplot() + geom_bar(stat="identity", aes(x = reorder(wide_location, -mean_length*-1), y = mean_rating, fill = mean_gain)) + 
  geom_bar(stat= "identity", aes ( x= reorder(wide_location, -mean_length*-1), y = (mean_length*-1))) + coord_flip() +
  labs( y = "Average length (miles)                                                                                 Average trail rating",
        fill = "Average\nAltitude\nGain (ft)") +
  scale_y_continuous(name = "Average length (miles)                                                                                     Average trail rating",
                     breaks = seq(-11,5, by = 1),
                     labels = abs(seq(-11,5, by = 1))) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(face= "bold", size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_text(size= 12, face = "bold"))  +
  scale_fill_viridis_c(option = "E")
   

