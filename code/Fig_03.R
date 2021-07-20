## Load packages
library(tidyverse)
library(ggforce)
library(extrafont)
library(grid)
library(gridExtra)

source("code/aff_amber_wos.R")
source("code/aff_no_amber_wos.R")

affs <- rbind(cbind(affs_amber, amber="yes"),
      cbind(affs_no_amber, amber="no")
)

affs$country[affs$country %in% c("England", "Scotland", "Wales")] <- "United Kingdom"


## get data in shape
affs <- affs %>%
  select(id, country, amber) %>% 
  distinct()

affs <- table(affs$country, affs$amber) %>%  data.frame()
colnames(affs) <- c("country", "amber", "count")
affs$country <- as.character(affs$country)

# Get centroid of locations
world_map <- map_data("world")
n <- which(world_map$subregion == "Hong Kong")
world_map$region[n] <- "Hong Kong"

world_map$code <- countrycode::countrycode(world_map$region, "country.name", "iso3c")
affs$code <- countrycode::countrycode(affs$country, "country.name", "iso3c")

# Retrievethe map data
countries <- subset(world_map, code %in% unique(affs$code))

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- countries %>%
  group_by(code) %>%
  summarise(long = mean(long), lat = mean(lat))

affs <- merge(affs, region.lab.data)

aff.plot <- affs %>%  arrange(country, desc(amber))

aff.plot$start <- rep(c(-pi/2, pi/2), nrow(affs)/2)

r <- 15 #radius of biggest circle
scale <- r/max(sqrt(aff.plot$count))

theme_set(theme_void(base_size=10) %+replace%
            theme(legend.title = element_text(
              face="bold", size=8),
              legend.text = element_text(
                size=8),
              #text=element_text(family="Roboto"),
              axis.title = element_text(face="bold"),
              legend.position="bottom",
              plot.background = element_rect(fill="white", colour=NA),
              plot.title = element_text(face="bold", hjust=0, size=14))
)

countries.lab <- aff.plot[aff.plot$count > 20 & aff.plot$amber == "yes",]$country
countries.lab <- c(countries.lab, "Myanmar")

omitc <- c("France","Germany", "Italy", "Poland", "Spain", "United Kingdom",
           "Czech Republic")
countries.lab <- countries.lab[!countries.lab %in% omitc]


p <- ggplot() +
  geom_polygon(data=world_map, aes(x = long, y = lat, group = group), 
               fill="#ded4c5ff", colour = "white", size=0.3) +
  geom_polygon(data=world_map[world_map$region == "Myanmar",], aes(x = long, y = lat, group = group), 
               fill="#dd9c69ff", colour = "#d07120ff", size=0.3) +
  geom_arc_bar(data=aff.plot, aes(x0 = long, y0 = lat, r0 = 0, r = sqrt(count)*scale,
                                  start = start, end = start + pi, fill = amber), alpha=0.9,
               color = "#f5f5ef", size=0.3) +
  labs(x="", y="", fill="Publications based \non amber inclusions") +
  scale_fill_manual(values=c("#2d1b0cff", "#d07120ff"), labels=c("No", "Yes")) +
  geom_text(data = aff.plot[aff.plot$country %in% countries.lab &  
                              duplicated(aff.plot$country)== FALSE,],
            aes(label = country, x = long, y = lat + scale*sqrt(count) + .05),
            size =2.5, vjust = 0, hjust=-0.2, family="Roboto", fontface=2) +
  
  geom_text(data = aff.plot[aff.plot$amber == "no" & aff.plot$country %in% countries.lab,],
            aes(label = count, x = long, y = lat + scale*sqrt(count) + .05), size=2, col="#d07120ff", 
            vjust = 4, hjust=0.5, family="Roboto", fontface=2) +
  geom_text(data = aff.plot[aff.plot$count > 20 & aff.plot$amber == "yes" & !aff.plot$country %in% omitc,],
            aes(label = count, x = long, y = lat + scale*sqrt(count) + .05), size=2, col="#2d1b0cff", 
            vjust = 2, hjust=0.5, family="Roboto", fontface=2) +
  coord_equal(ratio = 1.2)



# Europe

r2 <- 5 #radius of biggest circle
scale2 <- r2/max(sqrt(aff.plot$count))

p_eu <- ggplot() +
  geom_polygon(data=world_map, aes(x = long, y = lat, group = group), 
               fill="#ded4c5ff", colour = "white", size=0.5) +
  geom_arc_bar(data=aff.plot, 
               aes(x0 = long, y0 = lat, r0 = 0, r = sqrt(count)*scale2,
                   start = start, end = start + pi, fill = amber), alpha=0.9,
               color = "#f5f5ef", size=0.5) +
  labs(x="", y="", fill="") +
  scale_fill_manual(values=c("#2d1b0cff", "#d07120ff"), guide=FALSE)+
  geom_text(data = aff.plot[duplicated(aff.plot$country)==FALSE & aff.plot$count > 5,],
            aes(label = country, x = long, y = lat + scale2*sqrt(count) + .05),
            size =2.5, vjust = 4, hjust=-0.2, family="Roboto", fontface=2) +
  
  # number labels for no amber
  geom_text(data = aff.plot[aff.plot$amber == "no"& aff.plot$count > 5,],
            aes(label = count, x = long, y = lat+ scale2*sqrt(count) + .05), size=2, col="#d07120ff", 
            vjust = -5, hjust=0.5, family="Roboto", fontface=2) +
  #number labels for amber
  geom_text(data = aff.plot[aff.plot$amber == "yes"& aff.plot$count > 5,],
            aes(label = count, x = long, y = lat+ scale2*sqrt(count) + .01), 
            size=2, col="#2d1b0cff", 
            vjust = 0, hjust=1, family="Roboto", fontface=2) +
  coord_equal(ratio = 1.2, xlim=c(-9.5, 28.2), ylim=c(35, 61)) +
  theme(plot.background = element_rect(colour="black"))

layout=rbind(c(1, 1, 1,1,1,1),
             c(2, 2, 1,1,1,1),
             c(2, 2, 1,1,1,1))


map <- grid.arrange(p + annotate("rect", xmin=-9.5, xmax=28.2, ymin=35, ymax=61, fill=NA, col="#2d1b0cff"), p_eu, 
                    layout_matrix = layout)


ggsave(file.path("plots", "map_semi_circles.svg"), p, w=8, height=5)

svg(file.path("plots", "map_semi_circles_eu.svg"),width = 8, height=5)
plot(map) 
dev.off()


