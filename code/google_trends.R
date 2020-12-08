# _________________________
#
#  Google trends
#
# _________________________

library(tidyverse)
library(gtrendsR)
library(zoo)


## Import full dataset
google_data <-  read.csv("./data/google_trends2.csv", skip = 2, header = T)
names(google_data)[2] <- "hits" # rename second column
str(google_data) # have a peek

## Convert column to date format:
google_data$Week <- lubridate::dmy(google_data$Week)
google_data$Month <- as.Date(as.yearmon(google_data$Month))

str(google_data) # have a peek


trend_decade <- ggplot(google_data, aes(x=Month, y=hits)) +
  geom_line(colour = "#CE6B12", size = 1.5) +
  geom_ribbon(aes(ymin = 0, ymax = hits), fill = "#CE6B12", alpha = 0.4) +
  theme_minimal() + labs(x = "", y = "Number of hits") +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 105, 20), expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
trend_decade

ggsave("./plots/ggogletrends.pdf", plot = trend_decade, width = 30, height = 12, units = "cm")



#### Using gtrendsR
search1 <- gtrends(c("Myanmar amber"), time= "all")
plot(search1)

over_time <- search1$interest_over_time

q <- ggplot(over_time, aes(x=date, y=hits)) +
  geom_line() + 
  xlab("")
q



