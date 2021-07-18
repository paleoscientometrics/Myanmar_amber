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

yrs <- 2015
pre2015 <- which(all$Publication.Year < yrs)
pre2015 <- affs[affs$id %in% pre2015,]
pre2015 <- pre2015[,c("id", "country")]
pre2015 <- unique(pre2015)

table(pre2015$country)
table(pre2015$country) / length(unique(pre2015$id))

post2015 <- which(all$Publication.Year > 2015)
post2015 <- affs[affs$id %in% post2015,]
post2015 <- post2015[,c("id", "country")]
post2015 <- unique(post2015)

table(post2015$country)
table(post2015$country) / length(unique(post2015$id))

