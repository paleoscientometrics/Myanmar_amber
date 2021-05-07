## Load packages
library(tidyverse)
library(ggforce)
library(extrafont)
library(grid)
library(gridExtra)

## Import subset of "affilation data" that has Myanmar as the sampling country:
aff_data <- read.csv(file.path("data", "aff_data_Myanmar.csv"))
aff_data <- data.frame(apply(aff_data, 2, function(x) gsub("United States", "USA", x)))

colnames(aff_data)[1] <- "reference_no"

# reshape affiliation data
affs <- reshape2::melt(aff_data %>% dplyr::select(-pubyr, -author1last, -author2last, 
                                                  -reftitle, -doi, -publication_type, -complete_aff, -grid_code), 
                       id.vars=c("reference_no", "samp_country"), 
                       value.name="aff_country") %>% 
  dplyr::select(-variable) %>%
  filter(!aff_country %in% c("Unknown", ""))


## add column "amber" to indicate if the ref title contains a mention of 'amber'
aff_data$amber <- ifelse(grepl("amber", aff_data$reftitle, ignore.case = T), "yes", "no")

affs <- na.omit(merge(affs,aff_data[,c("reference_no", "amber")]))
affs <- affs %>% group_by(aff_country, amber) %>% 
  tally() %>% 
  pivot_wider(names_from = amber,
              values_from = n) %>% 
  mutate(yes = replace_na(yes, 0),
         no = replace_na(no, 0)
  )

affs <- affs %>% pivot_longer(c(yes, no), 
                              names_to="amber",
                              values_to="count")

# Get centroid of locations
world_map <- map_data("world")
n <- which(world_map$subregion == "Hong Kong")
world_map$region[n] <- "Hong Kong"

# Retrievethe map data
countries <- subset(world_map, region %in% unique(affs$aff_country))

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- countries %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

affs <- merge(affs, region.lab.data, by.x="aff_country", by.y="region")

aff.plot <- affs %>%  arrange(aff_country, desc(amber))

aff.plot$start <- rep(c(-pi/2, pi/2), nrow(affs)/2)

r <- 15 #radius of biggest circle
scale <- r/max(sqrt(aff.plot$count))

theme_set(theme_void(base_size=10) %+replace%
            theme(legend.title = element_text(
              face="bold", size=8),
              legend.text = element_text(
                size=8),
              text=element_text(family="Poppins"),
              axis.title = element_text(face="bold"),
              legend.position="bottom",
              plot.title = element_text(face="bold", hjust=0, size=14))
)

countries.lab <- aff.plot[aff.plot$count > 20 & aff.plot$amber == "yes",]$aff_country
countries.lab <- c(countries.lab, "Myanmar")

p <- ggplot() +
  geom_polygon(data=world_map, aes(x = long, y = lat, group = group), 
               fill="grey80", colour = "#ffffff", size=0.3) +
  geom_polygon(data=world_map[world_map$region == "Myanmar",], aes(x = long, y = lat, group = group), 
               fill="#dd211dff", colour = "#dd211dff", size=0.3) +
  geom_arc_bar(data=aff.plot, aes(x0 = long, y0 = lat, r0 = 0, r = sqrt(count)*scale,
                                  start = start, end = start + pi, fill = amber), alpha=0.9,
               color = "#f5f5ef", size=0.3) +
  labs(x="", y="", fill="Publications based \non amber inclusions",
       subtitle="A comparison between fieldwork done on amber vs non-amber fossils") +
  scale_fill_manual(values=c("black", "#dd211dff"), labels=c("No", "Yes")) +
  geom_text(data = aff.plot[aff.plot$aff_country %in% countries.lab &  duplicated(aff.plot$aff_country)== FALSE,],
            aes(label = aff_country, x = long, y = lat + scale*sqrt(count) + .05),
            size =2.5, vjust = 0, hjust=-0.2, family="Poppins", fontface=2) +
  
  geom_text(data = aff.plot[aff.plot$amber == "no" & aff.plot$aff_country %in% countries.lab,],
            aes(label = count, x = long, y = lat + scale*sqrt(count) + .05), size=2, col="#dd211dff", 
            vjust = 4, hjust=0.5, family="Poppins", fontface=2) +
  geom_text(data = aff.plot[aff.plot$count > 20 & aff.plot$amber == "yes",],
            aes(label = count, x = long, y = lat + scale*sqrt(count) + .05), size=2, col="#dd211dff", 
            vjust = 2, hjust=0.5, family="Poppins", fontface=2) +
  coord_equal(ratio = 1.2) +
  ggtitle("Locations of researchers carrying out fieldwork in Myanmar")

p

# Europe

r2 <- 5 #radius of biggest circle
scale2 <- r2/max(sqrt(aff.plot$count))

p_eu <- ggplot() +
  geom_polygon(data=world_map, aes(x = long, y = lat, group = group), 
               fill="grey80", colour = "white", size=0.5) +
  geom_arc_bar(data=aff.plot, 
               aes(x0 = long, y0 = lat, r0 = 0, r = sqrt(count)*scale2,
                   start = start, end = start + pi, fill = amber), alpha=0.9,
               color = "#f5f5ef", size=0.5) +
  labs(x="", y="", fill="") +
  scale_fill_manual(values=c("black", "#dd211dff"), guide=FALSE)+
  geom_text(data = aff.plot[duplicated(aff.plot$aff_country)==FALSE & aff.plot$count > 5,],
            aes(label = aff_country, x = long, y = lat + scale*sqrt(count) + .05),
            size =2.5, vjust = 4, hjust=-0.2, family="Poppins", fontface=2) +
  
  # number labels for no amber
  geom_text(data = aff.plot[aff.plot$amber == "no"& aff.plot$count > 5,],
            aes(label = count, x = long, y = lat + scale*sqrt(count) + .05), size=2, col="#d07120ff", 
            vjust = -5, hjust=0.5, family="Poppins", fontface=2) +
  #number labels for amber
  geom_text(data = aff.plot[aff.plot$amber == "yes"& aff.plot$count > 5,],
            aes(label = count, x = long, y = lat + scale*sqrt(count) + .05), 
            size=2, col="#2d1b0cff", 
            vjust = 0, hjust=1, family="Poppins", fontface=2) +
  coord_equal(ratio = 1.2, xlim=c(-9.5, 28.2), ylim=c(35, 61)) 

layout=rbind(c(1, 1, 1,1,1,1),
             c(2, 2, 1,1,1,1),
             c(2, 2, 1,1,1,1))


map <- grid.arrange(p + annotate("rect", xmin=-9.5, xmax=28.2, ymin=35, ymax=61, fill=NA, col="#2d1b0cff"), p_eu, 
                    layout_matrix = layout)


ggsave(file.path("plots", "map_semi_circles.svg"), p, w=8, height=5)

svg(file.path("plots", "map_semi_circles_eu.svg"),width = 8, height=5)
plot(map) 
dev.off()
