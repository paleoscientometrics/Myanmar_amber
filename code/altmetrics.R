
# Data --------------------------------------------------------------------
fnames <- list.files(path="output", "alt") #get filenames

#categories
cats <- gsub("alt_|\\.csv|_paleo", "", fnames)
cats <- do.call(rbind,strsplit(cats, "_"))

alt.df <- list()

for(i in seq_along(fnames)){
  alt.df[[i]] <- read.csv(file.path("output", fnames[i]))
  alt.df[[i]]$journal <-cats[i,1]
  alt.df[[i]]$topic <-cats[i,2]
}

alt.df <- data.table::rbindlist(alt.df, fill=TRUE) # all alt metric data

# Dino in title?
alt.df$dino <- "no"
alt.df$dino[grep("dino", alt.df$title, ignore.case = TRUE)] <- "yes"

#Altmetric summary
alt.summary <- alt.df %>%group_by(topic, dino) %>% 
  summarise(altmetric_score = mean(score), 
            tweets = mean(cited_by_tweeters_count, na.rm = TRUE),
            fb = mean(cited_by_posts_count, na.rm = TRUE))
