#https://www.webofscience.com/wos/woscc/summary/398c1e91-c98f-47fe-ad3c-bf37eab44aed-019a8e6d/relevance/1
# Paleontology, Geology, Zoology, Entomology, Evolutionary Biology

all <- read.csv("data/webofscience_amber.csv")
all <- all[all$Publication.Year>1989,]
min(all$Publication.Year, na.rm=T)
nrow(all)

sort(table(all$Source.Title))

# without affs
noaff <-all[-grep("^\\[", all$Addresses),]
nrow(noaff)

# all<- all[all$Publication.Year <2021,]
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

affs_amber <- affs

