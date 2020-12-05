# _________________________
#
#  PBDB affiliation data
#
# _________________________

## Load packages
library(tidyverse)
library(chronosphere) # fetch("pbdb")
library(cowplot)


## Import subset of "affilation data" that has Myanmar as the sampling country:
aff_data <- read.csv("./data/aff_data_Myanmar.csv")
glimpse(aff_data)

## add column "amber" to indicate if the ref title contains a mention of 'amber'
aff_data$amber <- ifelse(grepl("amber", aff_data$reftitle, ignore.case = T), "yes", "no")




# Get doi list ------------------------------------------------------------

## Subset to get doi's for amber papers:
amber_pubs <- subset(aff_data, amber == "yes")
str(amber_pubs)
doi_list <- as.vector(amber_pubs$doi)
doi_list <- doi_list[doi_list != ""]
doi_list <- as.data.frame(doi_list)

write_csv(doi_list, "./output/doi_list.csv")



# Geography of authors ----------------------------------------------------

#### Amber

## Subset to just amber papers:
amber_pubs <- as_tibble(subset(aff_data, amber == "yes"))
amber_pubs <- amber_pubs[!(amber_pubs$aff_country1 == "Unknown"),]


author1 <- as.vector(amber_pubs$aff_country1)
author2 <- as.vector(amber_pubs$aff_country2)
author3 <- as.vector(amber_pubs$aff_country3)
author4 <- as.vector(amber_pubs$aff_country4)
author5 <- as.vector(amber_pubs$aff_country5)
author6 <- as.vector(amber_pubs$aff_country6)
author7 <- as.vector(amber_pubs$aff_country7)

all_countries_a <- c(author1, author2, author3, author4, author5, author6, author7)
all_countries_a <- all_countries_a[all_countries_a != ""]
all_countries_a <- as.data.frame(all_countries_a)
names(all_countries_a)[1] <- "Country" 

countries_freq_a <- as.data.frame(table(all_countries_a$Country))
names(countries_freq_a)[1] <- "Country" # rename column headings
names(countries_freq_a)[2] <- "Freq"
head(countries_freq_a) # check


amber_countries <- ggplot(data=countries_freq_a, aes(x = reorder(Country, -Freq), y = Freq)) +
  geom_segment(aes(x=reorder(Country, -Freq), xend=reorder(Country, -Freq), y = 0, yend = Freq), color="#C89F7B", size=1.5) +
  geom_point(colour = "#CE6B12", size = 5) + # Canva social science lesson colour scheme
  theme_minimal() + theme(axis.text.x = element_text(angle=45, hjust=1)) + labs(x = "", y = "Frequency") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0))
amber_countries

#ggsave("./plots/pubs_per_yr_lollipop.pdf", plot = pubs_per_yr_lollipop, width = 25, height = 15, units = "cm")



#### Non-amber/body fossils

## Subset to just amber papers:
non_amber_pubs <- as_tibble(subset(aff_data, amber == "no"))
non_amber_pubs <- non_amber_pubs[!(non_amber_pubs$aff_country1 == "Unknown"),]

author1 <- as.vector(non_amber_pubs$aff_country1)
author2 <- as.vector(non_amber_pubs$aff_country2)
author3 <- as.vector(non_amber_pubs$aff_country3)
author4 <- as.vector(non_amber_pubs$aff_country4)
author5 <- as.vector(non_amber_pubs$aff_country5)
author6 <- as.vector(non_amber_pubs$aff_country6)
author7 <- as.vector(non_amber_pubs$aff_country7)

all_countries_na <- c(author1, author2, author3, author4, author5, author6, author7)
all_countries_na <- all_countries_na[all_countries_na != ""]
all_countries_na <- as.data.frame(all_countries_na)
names(all_countries_na)[1] <- "Country" 

na_countries_freq <- as.data.frame(table(all_countries_na$Country))
names(na_countries_freq)[1] <- "Country" # rename column headings
names(na_countries_freq)[2] <- "Freq"
head(na_countries_freq) # check

non_amber_countries <- ggplot(data=na_countries_freq, aes(x = reorder(Country, -Freq), y = Freq)) +
  geom_segment(aes(x=reorder(Country, -Freq), xend=reorder(Country, -Freq), y = 0, yend = Freq), color="#6BB1A0", size=1.5) +
  geom_point(colour = "#31695C", size = 5) + # Canva social science lesson colour scheme
  theme_minimal() + theme(axis.text.x = element_text(angle=45, hjust=1)) + labs(x = "", y = "Frequency") +
  scale_y_continuous(limits = c(0, 42), breaks = seq(0, 40, 10), expand = c(0, 0))
non_amber_countries


plot_grid(amber_countries, non_amber_countries, 
          nrow = 2, 
          labels = c('Amber', 'Non-amber'))

