library(tidyverse)
library(circlize)

source("code/aff_amber_wos.R")

head(affs)
affs$country[affs$country %in% c("England", "Wales", "Scotland", "UK")] <- "United Kingdom"

res <- list()

ids <- unique(affs$id)

for(i in 1:length(ids)){
  
  temp <- affs[affs$id==ids[i],]
  
  nms <- apply(temp,1, function(x) strsplit(x[2], ";"))
  coun <- matrix(unlist(apply(temp, 1, function(x) rep(x[5], x[4]))), ncol=1)
  
  temp <- cbind.data.frame(nms=unlist(nms),
                           country=unlist(coun), id=ids[i], year=unique(temp$Publication.Year))
  res[[i]] <- temp[duplicated(temp$nms)==F,]
}

#### identify dominance
firstauthors <- unlist(lapply(res, function(x) x$country[1]))
firstyears <- unlist(lapply(res, function(x) x$year[1]))

firstauthors <- data.frame(lead=firstauthors, year=firstyears)
firstauthors$period <- ifelse(firstyears<2014, "pre", "post")

first2 <- firstauthors %>% group_by(lead, period) %>% 
  tally() %>% 
  group_by(period) %>% 
  mutate(total=sum(n),
         prop=n/total) %>%  
  pivot_wider(id_cols=lead, names_from=period, values_from=prop) %>% 
  arrange(desc(post, desc(pre)))



res3 <- list()

for(i in 1:length(res)){
  temp <- res[[i]]
  n <- length(unique(temp$country))
  
  if(nrow(temp)>1){
    if(n>1){
      t2 <- data.frame(t(combn(unique(temp$country), 2)))
      t2 <- t2[t2$X1 == firstauthors$lead[i],]
      t2$period <- firstauthors$year[i]
    } else {
      t2 <- data.frame(X1=unique(temp$country), X2=unique(temp$country), period=temp$year[1])
    }
    
    
    res3[[i]] <- t2
  } else res3[[i]] <- NA
}


res3 <- do.call(rbind.data.frame, res3)
colnames(res3) <- c("from", "to", "year")
res3 <- na.omit(res3)

#### new plot

chord_plot <- function(df, order, grid.col=NULL, col=NULL){
  
  #create basic diagram
  chordDiagram(df, #choose columns from, to and label
               order=order, #order according to pillars
               
               #set colours
               grid.col = grid.col, #set colours
               col=col,
               #keep the symmetry of the links
               symmetric = TRUE, 
               #arrows = unidirectional, type sets an offset at arrow start and arrow tip at end
               directional = 1, direction.type = c("diffHeight", "arrows"), 
               link.arr.type = "big.arrow", #arrow type
               diffHeight = uh(3, "mm"), #set offset height  
               
               #annotation
               annotationTrack = c("grid"), annotationTrackHeight = 0.1,  #grid: create only grid for project titles
               
               #add new track for pillar title later
               preAllocateTracks = list(
                 track.height = uh(4, "mm"),
                 track.margin = c(uh(4, "mm"), 0)
               ), 
               
               #transparency of colours
               transparency = 0.7
  )
  
  #add titles. circos.track() goes through every single sectors within specified track
  circos.track(track.index = 2, panel.fun = function(x, y) {
    
    #get sector name
    sector.index = get.cell.meta.data("sector.index")
    
    #get coordinates
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    
    perc <- get.cell.meta.data("cell.width")/360

    # Add country label
    circos.text(mean(xlim), mean(ylim)+1, ifelse(perc>0.01, sector.index, ""), cex=0.8,
                facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))

  }, 
  
  #no border
  bg.border = NA)
}

#pre2014 ####
preedges <- res3 %>%  
  filter(year < 2014) %>% 
  group_by(from, to) %>% 
  summarise(weight = n()) %>% 
  ungroup()

df1 <- preedges %>% 
  select(from, to, value=weight)# %>%   filter(from != to)

order_regs <- data.frame(edge=c(df1$from, df1$to))
order_regs$region <- countrycode::countrycode(order_regs$edge, "country.name", "continent")

order_regs <- order_regs %>% group_by(edge, region) %>% 
  tally() %>% 
  arrange(region, n)
n <- table(order_regs$region)
col <- c(Africa="#e4ceaf", Oceania="#d09259", Europe="#c8691c", 
         Americas="#78380c", Asia="#340e06")
grid.col <- rep(col[names(n)], n)
names(grid.col) <- NULL


#pre2014 ####
postedges <- res3 %>%  
  filter(year >= 2014) %>% 
  group_by(from, to) %>% 
  summarise(weight = n()) %>% 
  ungroup()

df2 <- postedges %>% 
  select(from, to, value=weight) #%>%  filter(from != to)

order_regs2 <- data.frame(edge=c(df2$from, df2$to))
order_regs2$region <- countrycode::countrycode(order_regs2$edge, "country.name", "continent")

order_regs2 <- order_regs2 %>% group_by(edge, region) %>% 
  tally() %>% 
  arrange(region, n)

n <- table(order_regs2$region)
grid.col2 <- rep(col[names(n)], n)
names(grid.col2) <- NULL

svg("plots/Fig_04_period.svg", w=8, h=6)

par(mfrow=c(1,2))
chord_plot(df=df1, order=order_regs$edge,
           grid.col=grid.col)
mtext("(a)",side=3,line=-1.5, 
      at=par("usr")[1]+0.05*diff(par("usr")[1:2]),
      cex=0.8)

chord_plot(df=df2, order=order_regs2$edge,
           grid.col=grid.col2)

mtext("(b)",side=3,line=-1.5, 
      at=par("usr")[1]+0.05*diff(par("usr")[1:2]),
      cex=0.8)
dev.off()


#### single country author
n <- unlist(lapply(res, function(x) cbind(n=length(unique(x$country)),
                                          year=x$year[1])))
single <- res[which(n==1)]
single <-unlist(lapply(single, function(x) unique(x$country)))
length(single)

sort(table(single))






