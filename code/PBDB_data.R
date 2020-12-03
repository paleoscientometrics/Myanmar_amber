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

## Subset to get doi's for amber papers:
amber_pubs <- subset(aff_data, amber == "yes")
str(amber_pubs)
doi_list <- as.vector(amber_pubs$doi)
doi_list <- doi_list[doi_list != ""]
doi_list <- as.data.frame(doi_list)

write_csv(doi_list, "./output/doi_list.csv")


