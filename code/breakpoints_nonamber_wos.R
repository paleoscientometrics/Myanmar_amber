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
davies.test(my.lm)


my.seg <- segmented(my.lm, 
                    seg.Z = ~ year, 
                    psi = 2016)

# additional breakpoints?
pscore.test(my.seg,~year, more.break = T) 
davies.test(my.seg)

my.seg <- segmented(my.lm, 
                    seg.Z = ~ year, 
                    psi = c(2004, 2016))

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


ggplot(my.model) +
  geom_vline(xintercept = my.lines, linetype = "dashed") +
  # geom_line(data=my.model, aes(x=year,y=count), size=1) +
  geom_line(aes(x=year, y=fit, col=cat)) +
  geom_point(aes(x=year, y=count, col=cat))

# Get affs
affs <- list()

for (i in 1:nrow(all)){
  j <- all$Addresses[i]
  
  
  nms <- regmatches(j, gregexpr("(?=\\[).*?(?<=\\])", j, perl=T))[[1]]
  nms <- gsub("\\[|\\]", "", nms)

  
  adds <- gsub("\\[.*?\\]", "", j)
  adds <- strsplit(adds, ";")[[1]]
  
  affs[[i]] <- tryCatch(cbind.data.frame(id=i, nms, adds, nauthors=unlist(lapply(strsplit(nms, ";"), length))),
                        error = function(e) return(NULL))
}

#remove NAs
affs <- Filter(Negate(is.null), affs)
length(affs)

affs <- do.call(rbind, affs)
affs$country <- sub('.*,\\s*', '', affs$adds)
affs$country[grep("USA",affs$country)]<- "USA"
affs$country[grep("Peoples R China",affs$country)] <- "China"

pre2015 <- which(all$Publication.Year < 2015)
pre2015 <- affs[affs$id %in% pre2015,]
pre2015 <- pre2015[,c("id", "country")]
pre2015 <- unique(pre2015)

table(pre2015$country) / length(unique(pre2015$id))

post2015 <- which(all$Publication.Year > 2015)
post2015 <- affs[affs$id %in% post2015,]
post2015 <- post2015[,c("id", "country")]
post2015 <- unique(post2015)

table(post2015$country)
table(post2015$country) / length(unique(post2015$id))

