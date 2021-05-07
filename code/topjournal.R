library(scholar)
library(tidyverse)
library(abbrevr)
library(RColorBrewer)
#devtools::install_github("patrickbarks/abbrevr")

pal <- brewer.pal(8, "Dark2")[c(3,6)]
theme_set(ggthemes::theme_hc())

topjournals <- read.table(file.path("data", "topjournals.txt"), sep="\t", header=T)
colnames(topjournals) <- c("pubs", "records", "perc")
topjournals$pubs[13] <- topjournals$pubs[11]
topjournals <- data.frame(tapply(topjournals$perc, topjournals$pubs, sum))
colnames(topjournals) <- "records"
topjournals$pubs <- row.names(topjournals)
row.names(topjournals) <- NULL
topjournals <- na.omit(topjournals)
topjournals <- topjournals[order(topjournals$records, decreasing = T),][1:25,]


metrics <- get_impactfactor(topjournals$pubs)

topjournals$jif <- -metrics$ImpactFactor
topjournals$pubs
topjournals <- topjournals[,c(2,1,3)] %>% pivot_longer(records:jif)

write.csv(topjournals, "data/topjournals.csv",row.names = F)

topjournals <- read.csv(file.path("data","topjournals2.csv"))
topjournals$value2 <- NA
topjournals$value2[topjournals$name=="jif"] <- topjournals$value[topjournals$name=="records"]

library(ggplot2)

p1 <- ggplot(topjournals[topjournals$name == "records",], aes(x=reorder(abbrev, value), y=value)) +
  geom_bar(stat="identity", fill=pal[1]) +
  ylim(0, 35)+
  coord_flip(expand = F) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_text(face="bold"),
        axis.ticks.length.y.left = unit("0", "cm"),
          axis.text.y = element_text(hjust=0.5)
  ) +
  labs(y="Percentage of publications")
p1


p2 <- ggplot(topjournals[topjournals$name == "jif",], 
       aes(x=reorder(abbrev, value2), y=value)) +
  geom_bar(stat="identity", fill=pal[2]) +
  scale_y_continuous(breaks = -seq(0,44, 10), labels=seq(0,44, 10))+
  coord_flip(expand = F) +
  theme(axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text(face="bold"),
        axis.ticks.length.y.left = unit("0", "cm")
  ) +
  labs(y="Impact factor") 

library(patchwork)
svg(file.path("plots", "journals.svg"), w=8, h=6)
p2 + p1
dev.off()
