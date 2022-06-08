## ---------------------------
##
## "Ethics, law, and politics in palaeontological research: 
##    The case of Myanmar amber"
## Dunne, Raja, et al. (2022) Commun. Bio.
##
## Purpose of script: Stats tests on affiliation data
##
## Author: Nussaïbah B. Raja
##
## Date Last Modified: 2022-06-08
##
## Copyright (c) Nussaïbah B. Raja (2022)
## Email: nussaibah.raja.schoob@fau.de
##
## ---------------------------
##
## Notes: N/A
##   
##
## ---------------------------

## Load packages:
library(tidyverse)


## Load data
amber <- read.csv("data/webofscience_amber.csv")
amber <- setNames(data.frame(table(amber$Publication.Year)),
                        c("year", "amber"))

noamber <- read.csv("data/webofscience_no_amber.csv")
noamber <- setNames(data.frame(table(noamber$Publication.Year)),
                  c("year", "noamber"))


period <- data.frame(start=c(1990, 2014, 2004),
                     end=c(2013, 2021, 2015))

period$fold <- NA

for(i in 1:nrow(period)){
df <- merge(amber, noamber) %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  filter(year>=period$start[i] & year <= period$end[i]) %>% 
  mutate(ratio = amber/noamber) 

period$fold[i] <- mean(df$ratio)
}

period$fold


## Run scripts for organising the data
source("code/00-aff_amber_wos.R")
source("code/00-aff_no_amber_wos.R")

affs <- rbind(cbind(affs_amber, amber="yes"),
                  cbind(affs_no_amber, amber="no")
)

affs$country[affs$country %in% c("England", "Scotland", "Wales", "UK")] <- "United Kingdom"

period <- data.frame(start=c(1990, 2014, 1990),
                     end=c(2013, 2020, 2020))

res <- matrix(NA, nrow=nrow(period), ncol=2)

for(i in 1:nrow(period)){

## get data in shape
temp <- affs %>%
  filter(Publication.Year >=period$start[i] & Publication.Year <=period$end[i]) %>% 
  dplyr::select(id, country, amber) %>% 
  distinct()

temp <- table(temp$country, temp$amber) %>%  data.frame()
colnames(temp) <- c("country", "amber", "count")
temp$country <- as.character(temp$country)

temp2 <- temp %>% pivot_wider(id_cols=country, names_from=amber, values_from=count )

tt <- wilcox.test(temp2$no, temp2$yes/7, paired = T, alternative = "two.sided")
res[i,] <- c(tt$statistic, tt$p.value)

}

colnames(res) <- c("V", "p")
cbind(period,res)
