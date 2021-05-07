# _________________________
#
#  Web of Science data
#
# _________________________

library(tidyverse)

## Import full dataset
WOS_data <-  read.csv("./data/webofscience.csv", skip = 4, header = T)
glimpse(WOS_data) # have a peek


## Subset to last 30 years:
WOS_data_30 <- subset(WOS_data, Publication.Year >+ 1989)
#hist(WOS_data_30$Publication.Year, breaks = 30)


## Make it faaaaaaancy ;)

pubs_per_yr <- ggplot(data=WOS_data_30, aes(Publication.Year)) +
  geom_histogram(col="#9B4F0C", fill="#CE6B12", bins = 30) + # Canva social science lesson colour scheme
  theme_minimal() + labs(x = "Publication year", y = "Number of publications") +
  scale_x_continuous(breaks = seq(1990, 2020, 5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 144, 20), expand = c(0, 0))
pubs_per_yr
ggsave("./plots/pubs_per_yr_hist.pdf", plot = pubs_per_yr, width = 30, height = 12, units = "cm")



## Lollipop, lollipop, oh lolly lolly lollipop :)

## Get frequency table for publications per year
pubs_year <- as.data.frame(table(WOS_data_30$Publication.Year))
names(pubs_year)[1] <- "Year" # rename column headings
names(pubs_year)[2] <- "total_pubs"
head(pubs_year)


pubs_per_yr_lollipop <- ggplot(data=pubs_year, aes(x=Year, y = total_pubs)) +
  geom_segment(aes(x=Year, xend=Year, y = 0, yend = total_pubs), color="#C89F7B", size=1.5) +
  geom_point(colour = "#CE6B12", size = 5) + # Canva social science lesson colour scheme
  theme_minimal() + labs(x = "", y = "Number of publications") +
  scale_x_discrete(breaks = seq(1990, 2020, 2)) +
  scale_y_continuous(limits = c(0, 155), breaks = seq(0, 155, 30), expand = c(0, 0))
pubs_per_yr_lollipop

ggsave("./plots/pubs_per_yr_lollipop.pdf", plot = pubs_per_yr_lollipop, width = 25, height = 15, units = "cm")


#PaleoPercs
WOS_summary <- WOS_data_30 %>% group_by(Publication.Year) %>% 
  tally() %>% 
  mutate(ra = caTools::runmean(n, 3, alg="C"))

theme_set(theme_minimal(base_size=14) %+replace%
            theme(legend.title = element_text(
              face="bold"),
              text=element_text(family="Roboto"),
              axis.title = element_text(face="bold"),
              legend.position="bottom",
              plot.background = element_rect(fill="#f5f5ef", colour=NA),
              plot.title = element_text(face="bold", hjust=0, size=16))
)

timeline <- read.csv(file.path("data", "timeline.csv"))
timeline <- timeline[timeline$Events != "",]

timeline <- merge(WOS_summary, timeline[,c(1,2)], by.x="Publication.Year", by.y="Year")

google <- read.csv(file.path("data", "google_trends.csv"), skip=2)
colnames(google) <- c("Date", "Freq")
google$year <- gsub(".*/(\\d+{4})$", "\\1", google$Date)

google_summary <- google %>% group_by(year) %>% 
  summarise(n=sum(Freq))

ggplot(data=WOS_summary, aes(x=Publication.Year, y=ra)) + 
  geom_line(size=1, col="#d07120ff") + 
  geom_point(size=3, col="#d07120ff") +
  geom_text(data=timeline, aes(x=Publication.Year, y=ra, label=Events), inherit.aes = FALSE)+
  labs(x="Year of publication", 
       y="Number of Publications",
       subtitle = "\nMoving average of 3 years to take into consideration the publication process.\n") +
  ggtitle("Number of publications on amber fossils") 

ggsave(file.path("plots", "PaleoPercs_timeline.svg"), 
       w=8, h=5
)

## PalAss

theme_set(theme_minimal(base_size=14) %+replace%
            theme(legend.title = element_text(
              face="bold"),
              axis.title = element_text(face="bold"),
              legend.position="bottom",
              plot.title = element_text(face="bold", hjust=0, size=16))
)

ggplot(data=WOS_summary, aes(x=Publication.Year, y=ra)) + 
  geom_line(size=1, col="#d07120ff") + 
  geom_point(size=3, col="#d07120ff") +
  labs(x="", y="Number of Publications")

ggsave(file.path("plots", "PalAss_timeline.svg"), 
       w=10, h=5
)




# Funding

funding_organisations <- WOS_data_30[grep("amber", WOS_data_30$Article.Title),]$Funding.Orgs
funding_organisations <- funding_organisations[funding_organisations!= ""]
funding_organisations <- unlist(strsplit(funding_organisations, ";"))

funding_organisations <- gsub("(\\[.*\\])", "", funding_organisations) # remove funding numbers


funders <- data.frame(table(funding_organisations), stringsAsFactors = FALSE)
funders$funding_organisations <- as.character(funders$funding_organisations)

funders$country <- NA
funders$country[grep("China|Zhejiang|Chang-jiang|Shandong|CAS|Gansu|Tibet|Chinese|Beijing|Jiangsu|Shanghai|Capital Normal University|Guizhou|Nanjing|Guangzhou|Qingdao|Heibei", funders$funding_organisations)] <- "China"
funders$country[grep("Russia", funders$funding_organisations)] <- "Russia"
funders$country[grep("Japan", funders$funding_organisations)] <- "Japan"
funders$country[grep("DFG|Berlin|Alexander von Humboldt|German|Volkswagen", funders$funding_organisations)] <- "Germany"
funders$country[grep("Czech|Prague", funders$funding_organisations)] <- "Czechia"
funders$country[grep("Canada|Alberta", funders$funding_organisations)] <- "Canada"
funders$country[grep("Spain|Spanish", funders$funding_organisations)] <- "Spain"
funders$country[grep("NSF|U\\.S\\.|Utah|Paleontological Society|New York|Tennessee|US", funders$funding_organisations)] <- "USA"
funders$country[grep("Poland|Polish|Katowice", funders$funding_organisations)] <- "Poland"
funders$country[grep("CNRS|FNRS|Rennes|France|French", funders$funding_organisations)] <- "France"
funders$country[grep("Australia|Adelaide|Sydney", funders$funding_organisations)] <- "Australia"
funders$country[grep("CNPq|FAPESP", funders$funding_organisations)] <- "Brazil"
funders$country[grep("Slovak", funders$funding_organisations)] <- "Slovakia"
funders$country[grep("Europe", funders$funding_organisations)] <- "EU"
funders$country[grep("King Saud", funders$funding_organisations)] <- "Saudi Arabia"
funders$country[grep("KwaZulu", funders$funding_organisations)] <- "South Africa"
funders$country[grep("UNESCO", funders$funding_organisations)] <- "UN"
funders$country[grep("Hungar", funders$funding_organisations)] <- "Hungary"
funders$country[grep("HKU", funders$funding_organisations)] <- "Hong Kong"
funders$country[grep("Leban", funders$funding_organisations)] <- "Lebanon"


funders$funding_organisations[is.na(funders$country)]

#manual
write.csv(funders, file.path("output", "funding_organisations.csv"), row.names = FALSE)

