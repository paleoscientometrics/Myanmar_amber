library(rvest)
library(dplyr)
# Get Nature (and sister journals)

url <- "https://www.nature.com/search?order=relevance&article_type=research&subject=palaeontology&date_range=2015-2020&page="


nature_links <- list()

pg_no <- 16
for(i in 1:pg_no){
  pg <- read_html(paste0(url, i))
  
  links <- pg %>% 
    html_nodes(xpath='//*[@class="clean-list"]') %>%
    html_nodes(xpath='//*[@class="mb20 pb20 cleared"]') 
  
  type <- links %>% html_node("p") %>% html_text()
  type <- trimws(gsub("\n", "", type))
  type <- unlist(lapply(type, function(x) trimws(strsplit(x, "\\|")[[1]][1])))
  
  pub <- links %>% html_nodes(xpath='//*[@class="grid grid-7 mq640-grid-12 mt10"]')
  
  
  res <- cbind(title=trimws(gsub("\n", "", links %>% 
                                   html_node("a") %>% html_text())), url=paste0("https://www.nature.com", links %>% 
                                                                                  html_node("a") %>% 
                                                                                  html_attr("href")), type=type,
               pubtitle = trimws(gsub("\n", "", pub %>%  html_node("a") %>%  html_text())), 
               vol = trimws(pub %>% html_node("span") %>%  html_text()))
  
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

for(i in 406:nrow(res)){
  pg <- read_html(res$url[i])
  
  temp <- pg %>% 
    html_nodes(xpath='//*[@class="c-bibliographic-information__value"]') %>% 
    html_text()
  
  res[i,c("received", "accepted", "published", "doi")] <- temp[c(1:3, length(temp))]
  
  tryCatch({res$ethics[i] <- pg %>%
    html_nodes(xpath='//*[@id="ethics-content"]') %>% 
    html_node("p") %>% 
    html_text()}, error=function (error) return(invisible(NA)))
  
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

#write.csv(res, file.path("output", "nature_all_paleo.csv"), row.names = FALSE)
openxlsx::write.xlsx(res,file.path("output", "nature_all_paleo.xlsx") ) #encoding purposes

