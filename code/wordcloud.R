library(wordcloud)
library(tm)
library(wordcloud2)
library(dplyr)
library(RColorBrewer)

nature <- readxl::read_excel(file.path("output", "nature_all_paleo.xlsx"))
science <- readxl::read_excel(file.path("output", "science_all_paleo.xlsx"))

#dates
dates <- c(as.Date(nature$published, "%d %B %Y"), 
           as.Date(science$published, "%d %b %Y"))
min(dates, na.rm=T)
max(dates, na.rm=T)

text <- c(nature$title, science$title)
# Create a corpus  
docs <- Corpus(VectorSource(text))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# creative a document matrix 
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234) # for reproducibility 

svg(file.path("plots", "wordcloud.svg"), w=6, h=5)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,           
          max.words=100, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))
dev.off()

write.csv(df[1:10,], "nature_top.csv", row.names = F) 
