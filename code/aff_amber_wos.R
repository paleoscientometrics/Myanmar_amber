all <- read.csv("data/webofscience_amber.csv")
all<- all[all$Publication.Year <2021,]
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

affs_amber <- affs
