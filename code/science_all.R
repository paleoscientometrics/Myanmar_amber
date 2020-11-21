library(RSelenium)
library(rvest)
library(dplyr)
library(rAltmetric)

url <- "https://search.sciencemag.org/?searchTerm=paleontology&order=tfidf&limit=textFields&pageSize=100&startDate=2015-01-01&endDate=2020-11-21&articleTypes=Research%20and%20reviews&"

fprof <- getFirefoxProfile("C:/Users/nussa/Desktop/mgkvqzwi.selen", useBase = TRUE)

# Download binaries, start driver, and get client object.
rd <- rsDriver(browser = "firefox", port = 4442L, 
               extraCapabilities = fprof
)

ffd <- rd$client
Sys.sleep(5)
pg_no <- 2

science_links <- list()

for(i in 1:pg_no){
  ffd$navigate(url)
  Sys.sleep(3)
  page <- read_html(ffd$getPageSource()[[1]])
  
  links <- page %>% 
    rvest::html_nodes(xpath = '//*[@class="media__headline"]')
  
  vols <- page %>% 
    rvest::html_nodes(xpath = '//*[@class="ss-media__meta list-inline"]') %>% 
    html_text()
  
  vols <- gsub("DOI\\: 10[\\.][0-9]{4,}/[a-z]+\\.[a-z0-9]+", "", vols) #remove DOI
  
  
  res <- cbind(title=links %>% html_text(), url=links %>% html_node("a") %>%  html_attr("href"), type="Research & Reviews",
               pubtitle = gsub("(.*) Vol.*", "\\1", vols), 
               vol = gsub(".*Vol\\. ([0-9]+),.*", "\\1", vols))
  
  science_links[[i]] <- res
  # click on next
  load_btn <- ffd$findElement(using = "css selector", ".ss-pager__next")
  load_btn$clickElement()
}



res <- do.call(rbind.data.frame, science_links)

ffd$close()
rd$server$stop()
rm(rd)
gc()

# get almetrics -----------------------------------------------------------

res$received <- NA
res$accepted <- NA
res$published <- NA
res$doi <- NA
res$ethics <- NA #not found for science

alt_mets <- list()

for(i in 1:nrow(res)){
  pg <- read_html(paste0(res$url[i], "/tab-article-info"))
  
  doi <- pg %>% 
    html_nodes(xpath='//*[@class="panel-pane pane-highwire-doi"]') %>% 
    html_text()
  
  doi <- trimws(gsub("\n|DOI\\:|https://doi.org/", "", doi))
  res$doi[i] <- gsub(".*(10.*$)", "\\1", doi)
  
  temp <- pg %>% 
    html_nodes(xpath='//*[@class="publication-history"]') %>% 
    html_text()
  
  res[i,c("received", "accepted")] <- trimws(gsub("Received for publication|\\.|,", "", strsplit(temp, "Accepted for publication")[[1]]))
  
  trimws(gsub("Received for publication", "", strsplit(temp, "Accepted for publication")[[1]]))
  
  pub <- pg %>% 
    html_nodes(xpath='//*[@class="meta-line"]') %>% 
    html_text()
  
  res$published[i] <- gsub(".*([0-9]{2} [A-z]{3} [0-9]{4}).*", "\\1", pub)
  
  tryCatch({alt_temp <- rAltmetric::altmetrics(doi=res$doi[i])
  alt_mets[[i]] <- cbind(id=paste0("sci", i), rAltmetric::altmetric_data(alt_temp))}, error = function (error) message (error))
}

alt_mets <- alt_mets[lapply(alt_mets,length)>0] #remove empty containers


#write.csv(res, file.path("output", "nature_all_paleo.csv"), row.names = FALSE)
openxlsx::write.xlsx(cbind(id=paste0("sci", 1:nrow(res)), res),file.path("output", "science_all_paleo.xlsx") ) #encoding purposes
write.csv(data.table::rbindlist(alt_mets, fill=TRUE), file.path("output", "alt_science_all_paleo.csv"), row.names = FALSE)

