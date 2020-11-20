library(rvest)
library(dplyr)
# Get Nature (and sister journals) from Google Scholar

url <- c("https://www.nature.com/search?q=myanmar+amber&page=1", "https://www.nature.com/search?q=myanmar+amber&page=2")


nature_links <- list()

for(i in seq_along(url)){
  pg <- read_html(url[i])
  
  links <- pg %>% 
    html_nodes(xpath='//*[@class="clean-list"]') %>%
    html_nodes(xpath='//*[@class="mb20 pb20 cleared"]') 
  
  type <- links %>% html_node("p") %>% html_text()
  type <- trimws(gsub("\n", "", type))
  type <- unlist(lapply(type, function(x) trimws(strsplit(x, "\\|")[[1]][1])))
  
  
  res <- cbind(title=trimws(gsub("\n", "", links %>% 
                                   html_node("a") %>% html_text())), url=paste0("https://www.nature.com", links %>% 
                                                                                  html_node("a") %>% 
                                                                                  html_attr("href")), type=type)
  
  nature_links[[i]] <- res
  
}

res <- do.call(rbind.data.frame, nature_links)
res <- res[res$type == "Research",]

# Extra metadata ----------------------------------------------------------
res$received <- NA
res$accepted <- NA
res$published <- NA
res$doi <- NA
res$ethics <- NA
res$online <- NA
res$context<- NA 
res$mentions <- NA

for(i in 35:nrow(res)){
  pg <- read_html(res$url[i])
  
  temp <- pg %>% 
    html_nodes(xpath='//*[@class="c-bibliographic-information__value"]') %>% 
    html_text()
  
  res[i,c("received", "accepted", "published", "doi")] <- temp[c(1:3, length(temp))]
  
  res$ethics[i] <- pg %>%
    html_nodes(xpath='//*[@id="ethics-content"]') %>% 
    html_node("p") %>% 
    html_text()
  
  # Metrics
  pg <- read_html(paste0(res$url[i], "/metrics"))
  
  tryCatch({online <- pg %>% 
    html_nodes(xpath='//*[@class="c-article-metrics__legend"]') %>% 
    html_text()
  
  online <- trimws(strsplit(online, "\n")[[1]])
  res$online[i] <- paste(online[online != ""], collapse = ";")
  
  res$context[i] <- gsub("\\s+", " ", gsub("\n", " ", pg %>% 
                                             html_nodes(xpath='//*[@class="c-article-metrics__altmetric-context-score"]') %>% 
                                             html_text()))
  
  res$mentions[i] <- paste(pg %>% 
                             html_nodes(xpath='//*[@class="c-card"]') %>% 
                             html_node("span") %>% 
                             html_text(), collapse=";")}, error = function(error) return(invisible(NA)))
}

write.csv(res, file.path("output", "nature_myanmar.csv"), row.names = FALSE)
openxlsx::write.xlsx(res,file.path("output", "nature_myanmar.xlsx") ) #encoding purposes
