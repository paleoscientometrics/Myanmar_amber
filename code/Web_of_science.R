# _________________________
#
#  Web of Science data
#
# _________________________
install.packages("esquisse")
esquisse::esquisser()
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



## Lollipop, lollipop, oh lolly lolly lollipop :)

pubs_per_yr_lollipop <- ggplot() +
  geom_col(data=WOS_data_30, aes(x=Publication.Year, y = 1:nrow(WOS_data_30))) +
  geom_point(data = WOS_data_30, aes(Publication.Year, 1:nrow(WOS_data_30)), colour = "orange", size = 4) +
  #geom_histogram(col="#9B4F0C", fill="#CE6B12", bins = 30) + # Canva social science lesson colour scheme
  theme_minimal() + labs(x = "Publication year", y = "Number of publications") #+
  #scale_x_continuous(breaks = seq(1990, 2020, 5), expand = c(0, 0)) +
  #scale_y_continuous(breaks = seq(0, 144, 20), expand = c(0, 0))
pubs_per_yr_lollipop


flights_delay %>%
  ggplot(aes(x=carrier,y=avg_arr_delay)) +
  geom_col()+
  labs(y= "Mean Arrival Delay", x="Carrier")






