## ---------------------------
##
## "Ethics, law, and politics in palaeontological research: 
##    The case of Myanmar amber"
## Dunne, Raja, et al. (2022) Commun. Bio.
##
## Purpose of script: Creating Fig. 1 (map of Myanmar)
##
## Authors: Nussaïbah B. Raja
##
## Date Last Modified: 2022-06-07
##
## Copyright (c) Nussaïbah B. Raja, 2022
## Email: nussaibah.raja.schoob@fau.de
##
## ---------------------------
##
## Notes: N/A
##   
##
## ---------------------------


library(ggplot2)
library(sf)
library(extrafont)
library(dplyr)

loadfonts()

# Data ---------------------------------------------------------------------
myanmar_provinces <- st_read(file.path("shp", "state boundaries"))
kachin <- myanmar_provinces[myanmar_provinces$ST == "Kachin",]

hukawng_valley <- c(96.558889,26.438333) 

locations <- data.frame(loc = c("Tanai", "Myitkyina", "Tengchong"),
                        x=c(96.716667, 97.4, 98.483333),
                          y=c(26.35, 25.383333, 25.016667)
                        )

countries <- c("Myanmar", "China", "India", "Thailand", "Laos", "Bangladesh", "Nepal", "Bhutan")

myanmar_map <- map_data("world", region = countries)

# Compute the centroid as the mean longitude and latitude
# Used as label coordinate for country's names
region.lab.data <- myanmar_map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))


myanmar_map$fill <- ifelse(myanmar_map$region == "Myanmar", "yes", "no")

theme_set(theme_void(base_size=10) %+replace%
            theme(legend.title = element_text(
              face="bold", size=8),
              legend.text = element_text(
                size=8),
              text=element_text(family="Roboto"),
              axis.title = element_blank(),
              legend.position="bottom",
              plot.background = element_rect(fill="#f5f5ef", colour=NA),
              plot.title = element_text(face="bold", hjust=0, size=14))
)

myanmar_loc <- ggplot() +
  geom_polygon(data=myanmar_map, aes(x = long, y = lat,group = group, fill = fill), col="#f5f5efff", size=0.8) +
  scale_fill_manual(values=c("#ded4c5ff", "#e0c5acff"), guide=FALSE) +
  geom_text(data=region.lab.data, aes(x=long, y=lat, label=region), fontface=2, size=3, col="#2d1b0cff") +
  coord_map(xlim=c(80, 110), ylim=c(5.5, 35)) +
  annotate("rect", xmin=93, xmax=100, ymin=22, ymax=28.5, fill=NA, col="#2d1b0cff", size=0.5)

kachin_map <- ggplot() +
	geom_polygon(data=myanmar_map, aes(x = long, y = lat,group = group, fill = fill), col="#f5f5efff") +
  
  #fill kachin state
  geom_polygon(data=kachin, aes(x = long, y = lat, group = group), colour = "#f5f5efff", fill = "#dfaa81ff") +
  
  #add locations
  geom_point(data=locations, aes(x=x, y=y), size=5, col="#1d1207ff") +
  geom_text(data=locations, aes(x=x, y=y, label=loc), size=4, 
            col="#1d1207ff", hjust=-0.2, fontface=2) +
  
  scale_fill_manual(values=c("#ded4c5ff", "#e0c5acff"), guide=FALSE) +
	coord_map(xlim=c(93, 100), ylim=c(22, 28.5))+
  
  #annotate
  annotate("point", x=hukawng_valley[1], y=hukawng_valley[2], shape=1, size=8, stroke=1.2, colour="#834614ff") +
  annotate("text", x=hukawng_valley[1], y=hukawng_valley[2], label="Hukawng Valley", colour="#834614ff", hjust=1.1, vjust=-1, fontface=2)


ggsave(file.path("plots", "myanmar_loc.svg"), myanmar_loc, w=3)
ggsave(file.path("plots", "kachin_loc.svg"), kachin_map, w=8)


