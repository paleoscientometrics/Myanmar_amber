# _________________________
#
#  PBDB affiliation data
#
# _________________________

## Load packages
library(tidyverse)
library(chronosphere) # fetch("pbdb")



## Import subset of "affilation data" that has Myanmar as the sampling country:
aff_data <- read.csv("./data/aff_data_Myanmar.csv")
glimpse(aff_data)

## add column "amber" to indicate if the ref title contains a mention of 'amber'
aff_data$amber <- ifelse(grepl("amber", aff_data$reftitle, ignore.case = T), "yes", "no")
View(aff_data)
