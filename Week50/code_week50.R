library(tidyverse)
library(ggforce)
library(countrycode)

women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')
women<-women[-1,]



#get continent data
women$continent <- countrycode(sourcevar = women$country,
                            origin = "country.name",
                            destination = "continent")

#fix up some of the continents
women$continent[women$country == "Northern Ireland"] <- "Europe"
women$continent[women$country == "Wales, UK"] <- "Europe"
women$continent[women$country == "Exiled Uighur from Ghulja (in Chinese, Yining)"] <- "Asia"

#make women column
women$women<-rep("Women", nrow(women))


#let's make an alluvial plot
women2 <- gather_set_data(women, c(3,8)) 
women2$x <- factor(women2$x,
                levels = c("women", "category"))

ggplot(women2, aes(x, id = id, split = y, value = name_leng)) +
  geom_parallel_sets(aes(fill = continent), alpha = 0.6, axis.width = 0, n = 100,
                     strength = 0.6) +
  geom_parallel_sets_axes(axis.width = 0.03, fill = "gray34") +
  labs(fill = "Continental Region:") +
  scale_fill_viridis_d() +theme_minimal() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 14, face= "bold"),
    legend.title = element_text(size = 14, face= "bold"),
    plot.background = element_rect(fill = "gray", color = "transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_blank()
    
  )+
  annotate(
    geom = "text",
    x = 2.35,
    y = 1450,
    label = "CREATIVITY",
    size = 12,
    family = "Times"
  ) +
  annotate(
    geom = "text",
    x = 2.35,
    y = 1150,
    label = "IDENTITY",
    size = 12,
    family = "Times"
  ) +  annotate(
    geom = "text",
    x = 2.35,
    y = 750,
    label = "KNOWLEDGE",
    size = 12,
    family = "Times"
  ) +
  annotate(
    geom = "text",
    x = 2.35,
    y = 250,
    label = "LEADERSHIP",
    size = 12,
    family = "Times"
  ) +  annotate(
    geom = "text",
    x = 0.75,
    y = 750,
    label = "BBC\n100\nWomen\nof\n2020",
    size = 14,
    face= "bold", 
    family = "Helvetica"
  ) 





