# _________________________
#
#  Google trends
#
# _________________________

library(tidyverse)
library(gtrendsR)


## Import full dataset
google_data <-  read.csv("./data/google_trends.csv", skip = 2, header = T)
names(google_data)[2] <- "hits" # rename second column
str(google_data) # have a peek

## Convert column to date format:
google_data$Week <- lubridate::dmy(google_data$Week)
str(google_data) # have a peek

## Using gtrendsR
search1 <- gtrends(c("Myanmar amber"), time= "all")
plot(search1)

over_time <- search1$interest_over_time

p <- ggplot(google_data, aes(x=Week, y=hits)) +
  geom_line() + 
  xlab("")
p


q <- ggplot(over_time, aes(x=date, y=hits)) +
  geom_line() + 
  xlab("")
q



