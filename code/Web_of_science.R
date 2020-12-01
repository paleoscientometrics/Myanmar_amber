# _________________________
#
#  Web of Science data
#
# _________________________

library(tidyverse)


## Import full dataset
WOS_data <-  read.csv("./data/webofscience.csv", skip = 4, header = T)
glimpse(WOS_data) # have a peek
hist(WOS_data$Publication.Year, breaks = 200) # crude histogram of all data


## Subset to last 30 years:
WOS_data_30 <- subset(WOS_data, Publication.Year >+ 1990)
hist(WOS_data_30$Publication.Year, breaks = 30)


## Make it faaaaaaancy ;)

pubs_per_yr <- ggplot(data=WOS_data_30, aes(Publication.Year)) +
  geom_histogram(col="#9B4F0C", fill="#CE6B12", bins = 30) + # Canva social science lesson colour scheme
  theme_minimal() + labs(x = "Publication year", y = "Number of publications") +
  scale_x_continuous(breaks = seq(1990, 2020, 5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 144, 20), expand = c(0, 0))
pubs_per_yr

ggsave("./plots/pubs_per_yr_hist.pdf", plot = pubs_per_yr, width = 35, height = 12, units = "cm")



## Lollipop, lollipop, oh lolly lolly lollipop :)

## Get frequency table for publications per year
pubs_year <- as.data.frame(table(WOS_data_30$Publication.Year))
names(pubs_year)[1] <- "Year" # rename column headings
names(pubs_year)[2] <- "total_pubs"
head(pubs_year)


pubs_per_yr_lollipop <- ggplot(data=pubs_year, aes(x=Year, y = total_pubs)) +
  geom_segment(aes(x=Year, xend=Year, y = 0, yend = total_pubs), color="grey75", size=1.5) +
  geom_point(colour = "#CE6B12", size = 5) + # Canva social science lesson colour scheme
  theme_minimal() + labs(x = "Publication year", y = "Number of publications") +
  scale_x_discrete(breaks = seq(1990, 2020, 2)) +
  scale_y_continuous(limits = c(0, 155), breaks = seq(0, 155, 30), expand = c(0, 0))
pubs_per_yr_lollipop

ggsave("./plots/pubs_per_yr_lollipop.pdf", plot = pubs_per_yr_lollipop, width = 27, height = 15, units = "cm")




