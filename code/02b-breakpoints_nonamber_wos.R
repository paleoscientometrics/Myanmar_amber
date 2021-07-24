#https://www.webofscience.com/wos/woscc/summary/2a75fa0c-3edc-4937-9823-a530d1e3be22-0152a0ec/relevance/1

all <- read.csv("data/webofscience_no_amber.csv")
nrow(all)

all_summary <- setNames(data.frame(table(all$Publication.Year)),
        c("year", "count"))
all_summary$year <- as.numeric(as.character(all_summary$year))


all_summary$ra <- caTools::runmean(all_summary$count, 3, alg="C")

plot(all_summary$year, all_summary$ra)

library(segmented)

my.lm <- lm(ra~year, data=all_summary)
pscore.test(my.lm)
dt <- davies.test(my.lm)
d1 <- c(dt$process[dt$process[,1]==dt$statistic,], p.value=dt$p.value)


my.seg <- segmented(my.lm, 
                    seg.Z = ~ year, 
                    psi = 2016)
d1 <- c(d1, actual=my.seg$psi[2])

# additional breakpoints?
pscore.test(my.seg,~year, more.break = T) 
dt <- davies.test(my.seg)

d2 <- c(dt$process[dt$process[,1]==dt$statistic,], p.value=dt$p.value)

my.seg <- segmented(my.lm, 
                    seg.Z = ~ year, 
                    psi = c(2004, 2016))
d2 <- c(d2, actual=my.seg$psi[1])

dt <- davies.test(my.seg)
d3 <- c(dt$process[dt$process[,1]==dt$statistic,], p.value=dt$p.value, actual=NA)


d <- rbind(d1, d2, d3)
write.csv(d, "output/breakpoint_wos_na.csv", row.names = F)

# get the breakpoints
my.seg$psi


my.fitted <- fitted(my.seg)
my.model <- data.frame(year = all_summary$year, 
                       count = all_summary$ra, fit=my.fitted)


yrs <- round(my.seg$psi[,2])
yrs

my.model$cat <- NA
my.model$cat[my.model$year <yrs[1]] <- "1"
my.model$cat[my.model$year >yrs[2]] <- "2"
my.model$cat[is.na(my.model$cat)] <- "3"

my.lines <- round(my.seg$psi[, 2])

library(ggplot2)
pal <- c("1"="#c06c13","2"="#ff9009","3"="#340e06")

library(ggthemes)
theme_set(theme_hc(base_size=14) %+replace%
            theme(legend.title = element_text(
              face="bold"),
              axis.title = element_text(face="bold"),
              axis.title.y = element_text(angle=90),
              legend.position="none",
              plot.title = element_text(face="bold", hjust=0, size=16))
)

yrs=1999:2003
mod1 <- lm(ra~year, data=all_summary[all_summary$year >=yrs[1] & all_summary$year <=max(yrs),])
y1 <- predict(mod1, newdata=data.frame(year=yrs))
df1 <- data.frame(year=yrs, fit=y1, cat="1")


yrs=2003:2015
mod2 <- lm(ra~year, data=all_summary[all_summary$year >=yrs[1] & all_summary$year <=max(yrs),])
y2 <- predict(mod2, newdata=data.frame(year=yrs))
df2 <- data.frame(year=yrs, fit=y2, cat="3")

yrs=2016:2021
mod3 <- lm(ra~year, data=all_summary[all_summary$year >=yrs[1] & all_summary$year <=max(yrs),])
y3 <- predict(mod3, newdata=data.frame(year=yrs))
df3 <- data.frame(year=yrs, fit=y3, cat="2")

df <- rbind(df1, df2, df3)

ggplot(my.model) +
  geom_vline(xintercept = my.lines, linetype = "dashed") +
  # geom_line(data=my.model, aes(x=year,y=count), size=1) +
  geom_line(data=df, aes(x=year, y=fit, col=cat)) +
  geom_point(aes(x=year, y=count, col=cat), size=4) +
  scale_color_manual(values=pal) +
  labs(x="Year", y="Number of publications")

ggsave("plots/Supplement/Fig_S1_breakpoint.png", w=6, h=4)
