# _________________________
#
#  Google trends
#
# _________________________

library(tidyverse)


## Import full dataset
google_data <-  read.csv("./data/google_trends.csv", skip = 2, header = T)
glimpse(google_data) # have a peek



