#https://www.webofscience.com/wos/woscc/summary/2a75fa0c-3edc-4937-9823-a530d1e3be22-0152a0ec/relevance/1

all <- read.csv("data/webofscience_no_amber.csv")
all <- all[all$Publication.Year>1989,]
min(all$Publication.Year, na.rm=T)

sort(table(all$Source.Title))
#all<- all[all$Publication.Year <2021,]
nrow(all)


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
affs$country <- gsub("\\.", "", affs$country)

all$id <- 1:nrow(all)
affs <- merge(affs, all[,c("id", "Publication.Year")])


affs_no_amber <- affs
