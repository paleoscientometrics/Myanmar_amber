period <- c(2004,2010)

amber <- read.csv("data/webofscience_amber.csv")
amber <- setNames(data.frame(table(amber$Publication.Year)),
                        c("year", "amber"))

noamber <- read.csv("data/webofscience_no_amber.csv")
noamber <- setNames(data.frame(table(noamber$Publication.Year)),
                  c("year", "noamber"))


df <- merge(amber, noamber) %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  filter(year>=period[1] & year <= period[2]) %>% 
  mutate(ratio = amber/noamber) 

mean(df$ratio)
