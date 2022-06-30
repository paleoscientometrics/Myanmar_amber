## ---------------------------
##
## "Ethics, law, and politics in palaeontological research: 
##    The case of Myanmar amber"
## Dunne, Raja, et al. (2022) Commun. Bio.
##
## Purpose of script: Creating Fig. 2 (temporal trends and breakpoints)
##
## Authors: Nussaïbah B. Raja, Emma M. Dunne
##
## Date Last Modified: 2022-06-07
##
## Copyright (c) Nussaïbah B. Raja & Emma M. Dunne (2022)
## Email: nussaibah.raja.schoob@fau.de, dunne.emma.m@gmail.com
##
## ---------------------------
##
## Notes: N/A
##   
##
## ---------------------------


### Packages:
library(tidyverse)
library(segmented)
library(ggthemes)


## Load Web of Science data - amber publications 
all <- read.csv("./data/webofscience_amber.csv")

## Get total for 2022 as of June 7th 2022:
n2022 <- table(all$Publication.Year)["2022"]
# n2021 <- table(all$Publication.Year)["2021"]
# n2020 <- table(all$Publication.Year)["2020"]

## Subset dataset to only pubs from 2021 and younger
all <- all[all$Publication.Year <2022,]
nrow(all)

all_summary <- setNames(data.frame(table(all$Publication.Year)),
                        c("year", "count"))
all_summary$year <- as.numeric(as.character(all_summary$year))

## rolling average
all_summary$ra <- caTools::runmean(all_summary$count, 3, alg="C")
## quick plot
plot(all_summary$year, all_summary$ra)


#### Stats testing

## fit linear model
my.lm <- lm(ra~year, data=all_summary)
pscore.test(my.lm)

## Davies test
dt <- davies.test(my.lm)
d1 <- c(dt$process[dt$process[,1]==dt$statistic,], p.value=dt$p.value)

my.seg <- segmented(my.lm, 
                    seg.Z = ~ year, 
                    psi = 2013)

d1 <- c(d1, actual=my.seg$psi[2])

# additional breakpoints?
pscore.test(my.seg,~year, more.break = T) 
dt <- davies.test(my.seg)
d2 <- c(dt$process[dt$process[,1]==dt$statistic,], p.value=dt$p.value, actual=NA)

d <- rbind(d1, d2)
write.csv(d, "output/breakpoint_wos21.csv", row.names = F)

# get the breakpoints
my.seg$psi


my.fitted <- fitted(my.seg)
my.model <- data.frame(year = all_summary$year, 
                       count = all_summary$ra, fit=my.fitted)


yrs <- round(my.seg$psi[,2])
yrs

my.model$cat <- NA
my.model$cat[my.model$year <yrs] <- "1"
my.model$cat[my.model$year >yrs] <- "2"
my.model$cat[is.na(my.model$cat)] <- "3"

my.lines <- round(my.seg$psi[, 2])


ggplot(my.model) +
  geom_vline(xintercept = my.lines, linetype = "dashed") +
  # geom_line(data=my.model, aes(x=year,y=count), size=1) +
  geom_line(aes(x=year, y=fit, col=cat)) +
  geom_point(aes(x=year, y=count, col=cat))

### PLOT #####
# At the breakpoint (break1), the segments b and c intersect

#b0 + b1*x = c0 + c1*x

b0 <- coef(my.seg)[[1]]
b1 <- coef(my.seg)[[2]]

# Important:
# the coefficients are the differences in slope in comparison to the previous slope
c1 <- coef(my.seg)[[2]] + coef(my.seg)[[3]]
break1 <- my.seg$psi[[2]]

#Solve for c0 (intercept of second segment):
c0 <- b0 + b1 * break1 - c1 * break1

# first line: 
#y = b0 + b1*x
#y = intercept1 + slope1 * x

x1 <- 1995: (yrs-1)
df1 <- all_summary[all_summary$year %in% x1,]
mod1 <- lm(ra~year, df1)
summary(mod1)
y1= predict(mod1, newdata = data.frame(year=x1))

# second line:
#y = c0 + c1*x
#y = intercept2 + slope2 * x

x2 <- 2015:2020
df2 <- all_summary[all_summary$year %in% x2,]
mod2 <- lm(ra~year + I(year^2), df2)
x3 <- 2015:2021
y2= predict(mod2, newdata = data.frame(year=x3))

my.lines <- round(my.seg$psi[, 2])

## Set plot theme
theme_set(theme_hc(base_size=14) %+replace%
            theme(legend.title = element_text(face="bold"),
              axis.title = element_text(face="bold"),
              axis.title.y = element_text(angle=90),
              legend.position="bottom",
              plot.title = element_text(face="bold", hjust=0, size=16))
)

p <- ggplot() +
  geom_vline(xintercept = my.lines, linetype = "dashed") +
  # geom_line(data=my.model, aes(x=year,y=count), size=1) +
  geom_line(data=data.frame(x1, y1, cat="1"), 
            aes(x=x1, y=y1, col=cat), size=1) +
  geom_line(data=data.frame(x3, y2, cat="2"), 
            aes(x=x3, y=y2, col=cat), size=1) +
  
  scale_x_continuous(limits=c(1988, 2027), 
                     breaks = seq(1990, 2020, 5)) +
  labs(x="Year", y="Number of publications")

## Load info for important dates/events
labs <- read.csv("./data/timeline.csv")
labs <- merge(labs, all_summary, by.x="x1", by.y="year", all.x=T, all.y=F)
labs$ra[labs$x1%in%c(1993, 1994, 1995)] <- 0

hj <- 0
hj2 <- 1

ny <- -10
pal <- c("1"="#c06c13","2"="#ff9009","3"="#340e06")


## Add text and stats to plot
p1 <- p +
  # Adding annotations for years
  geom_text(data=labs[labs$hj=="right",], aes(x=x1, y=y1, label=x1, hjust=hj), 
            size=5, hjust=hj2, vjust=0.5, col=pal[1], fontface=2) +
  geom_text(data=labs[labs$hj=="right",], aes(x=x1, y=y1, label=label2), 
            size=3,  vjust=0.5,
            nudge_y = ny, hjust=hj2, lineheight = 0.8, col=pal[1]) +
  geom_segment(data=labs[labs$hj=="right",], aes(x=x1, xend=x1,y=ra, yend=y1-20),
               col="darkgrey") +
  geom_text(data=labs[labs$hj=="left",], aes(x=x1, y=y1, label=x1, hjust=hj), 
            size=5, hjust=hj, vjust=1, col= pal[2], fontface=2) +
  geom_text(data=labs[labs$hj=="left",], aes(x=x1, y=y1, label=label2), 
            size=3,  vjust=1, col=pal[2],
            nudge_y = ny, hjust=hj, lineheight = 0.8) +
  geom_segment(data=labs[labs$hj=="left",], aes(x=x1, xend=x1,y=ra, yend=y1),
               col="darkgrey") +
  geom_point(data=my.model, aes(x=year, y=count, col=cat, shape=cat), 
             size=4, fill="white", stroke=2) +
  scale_shape_manual(values=c("1"=16,"2"=16,"3"=21), guide="none")+
  scale_color_manual(values=pal, guide="none") + 
  # Add stats
  annotate("text", x=2005, y=60, 
           label= paste0("italic(R) ^ 2 ==", format(summary(mod1)$adj.r, digits=3)),
           parse=T, hjust=hj, size=3)+
  annotate("text", x=2005, y=52, 
           label= paste0("italic(p) ==", format(anova(mod1)$'Pr(>F)'[1], digits=3)),
           parse=T, hjust=hj, size=3) +
  annotate("text", x=2025, y=15, 
           label= paste0("italic(R) ^ 2 ==", format(summary(mod2)$adj.r, digits=3)),
           parse=T, hjust=hj, size=3)+
  annotate("text", x=2025, y=7, 
           label= paste0("italic(p) ==", format(anova(mod2)$'Pr(>F)'[1], digits=3)),
           parse=T, hjust=hj, size=3) +
  annotate("segment", x=2016, xend=2014.5, y=10, yend=18, # breakpoint arrow
           arrow = arrow(type = "closed", length = unit(0.1, "inches"))
           ) +
  annotate("text", x=2016.1, y=8,
           label="breakpoint\nidentified", size=3, hjust=hj,
           lineheight=0.8) +
  # annotate("segment", x=2014, xend=2013, y=230, yend=230, 
  #          arrow = arrow(type = "closed", length = unit(0.1, "inches"))) +
  annotate("point", x=2022, y=n2022, size=4, shape=21, stroke=2, col=pal[2]) +
  annotate("text", x=2022, y=51,
           label="Total publications\nin first half of year", size=3, hjust=hj, lineheight=0.8)
p1


#x11(w=10, h=6);p1

ggsave("plots/Fig_02.pdf", p1, w=10, h=6)


