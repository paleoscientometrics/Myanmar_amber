## ---------------------------
##
## "Ethics, law, and politics in palaeontological research: 
##    The case of Myanmar amber"
## Dunne, Raja, et al. (2022) Commun. Bio.
##
## Purpose of script: Creating Figs. S2, S3, 4, and S4
##
## Author: Nussaïbah B. Raja
##
## Date Last Modified: 2022-06-08
##
## Copyright (c) Nussaïbah B. Raja (2022)
## Email: nussaibah.raja.schoob@fau.de
##
## ---------------------------
##
## Notes: N/A
##   
##
## ---------------------------


## Packages
library(tidyverse)
library(circlize)
library(patchwork)
library(ggthemes)
library(ggpubr)
library(igraph)


## Load data 
source("code/00-aff_amber_wos.R")
pal <- c("#c06c13","#ff9009","#340e06")

head(affs)
affs$country[affs$country %in% c("England", "Wales", "Scotland", "UK")] <- "United Kingdom"



# Get clean list of authors with country and year of publication

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


## Extract first authors 
firstauthors <- unlist(lapply(res, function(x) x$country[1]))
firstyears <- unlist(lapply(res, function(x) x$year[1]))

firstauthors <- data.frame(lead=firstauthors, year=firstyears)
firstauthors$period <- ifelse(firstyears<2014, "pre", "post")

first1 <- firstauthors %>% group_by(lead, period) %>% 
  tally() %>% 
  group_by(period) %>% 
  mutate(total=sum(n),
         prop=n/total) 

first3 <- first1 %>% 
  group_by(period) %>% 
  slice_max(n=10, order_by=prop)

first3 <- first3[1:20,]

theme_set(theme_hc(base_size=12) %+replace%
            theme(legend.title = element_text(
              face="bold"),
              axis.title = element_text(face="bold"),
              axis.title.y = element_text(angle=90),
              legend.position="bottom", 
              plot.tag = element_text(size=12))
)


p1 <- first3 %>% filter(period=="pre") %>% 
ggplot(aes(x=reorder(lead, prop), y=prop*100)) +
  geom_bar(stat="identity", fill=pal[1]) +
  coord_flip() +
  labs(y="% of first-author publications", x="")


p2 <- first3 %>% filter(period=="post") %>% 
  ggplot(aes(x=reorder(lead, prop), y=prop*100)) +
  geom_bar(stat="identity",fill=pal[2]) +
  coord_flip() +
  labs(y="% of first-author publications", x="")

png("plots/Supplement/Fig_S2.png", w=8, h=4, units="in", res=300)
p1+p2 +
  plot_annotation(tag_prefix = "(", tag_levels = "a", tag_suffix = ")")
dev.off()


## Outlier detection 

first2 <- first1 %>% pivot_wider(id_cols=lead, names_from=period, values_from=n) #%>% 
  #arrange(desc(post, desc(pre)))

first1$period <- factor(first1$period, levels=c("post", "pre"))

p3 <- ggplot(first1, aes(x = period, y = n, col=period)) +
  geom_boxplot(outlier.alpha = 0) +
  coord_flip() +
  scale_color_manual(values=pal[2:1]) +
  geom_jitter(size = 2, alpha = 0.25, 
              position = position_jitter(seed = 123, width = 0.2)) +
  scale_x_discrete(labels=c("post-2014","pre-2014")) +
  labs(y="Number of publications", x="") +
  theme(legend.position = "none")

x1 <- boxplot(first2$pre)
x2 <- boxplot(first2$post)

outx <- rbind(setNames(first2[first2$pre %in% x1$out,c("lead", "pre")], c("coun", "n")),
              setNames(first2[first2$post %in% x2$out,c("lead", "post")], c("coun", "n")))

outx$period <- c("pre", "post", "post", "post", "post", "post")
outx$y <- c(1.93, 1.05, 1.18, 0.8, 1.09, 0.9)

outx$n[2] <- 380
p3 <- p3 + annotate("text", y=outx$n, x=outx$y, label=outx$coun, size=3, hjust=-0.3)
ggsave("plots/Supplement/Fig_S3.png", p3, w=6, h=4)


## Collaborations

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


## Compute numbers
res4 <- do.call(rbind,
        lapply(res, function (x) cbind.data.frame(n=length(unique(x$country)), 
                                       year=x$year[1]))
)


incountry1 <- do.call(rbind, res[which(res4$n ==1 & res4$year < 2014)]) %>% 
  distinct(id, country) %>% 
  group_by(country) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  left_join(first2 %>%  dplyr::select(pre, country=lead)) %>% 
  mutate(prop=n/pre)
mean(incountry1$prop)

incountry <- do.call(rbind, res[which(res4$n ==1 & res4$year >= 2014)]) %>% 
  distinct(id, country) %>% 
  group_by(country) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  left_join(first2 %>%  dplyr::select(post, country=lead)) %>% 
  mutate(prop=n/post)
mean(incountry$prop)

#### single author
single <- do.call(rbind.data.frame,lapply(res, function(x) cbind(n=nrow(x),
                                                                 year=x$year[1])))

length(res[which(single$n==1 & single$year <2014)])/ length(unique(affs_amber$id[affs_amber$Publication.Year < 2014]))

length(res[which(single$n==1 & single$year >=2014)])/ length(unique(affs_amber$id[affs_amber$Publication.Year >= 2014]))



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
  dplyr::select(from, to, value=weight)# %>%   filter(from != to)

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
  dplyr::select(from, to, value=weight) #%>%  filter(from != to)

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




# General collaborations --------------------------------------------------
res4 <- list()

for(i in 1:length(res)){
  temp <- res[[i]]
  n <- length(unique(temp$country))
  
  if(nrow(temp)>1){
    if(n>1){
      t2 <- data.frame(t(combn(unique(temp$country), 2)))
      t2$period <- firstauthors$year[i]
    } else {
      t2 <- data.frame(X1=unique(temp$country), X2=unique(temp$country), period=temp$year[1])
    }
    
    
    res4[[i]] <- t2
  } else res4[[i]] <- NA
}


res4 <- do.call(rbind.data.frame, res4)
colnames(res4) <- c("from", "to", "year")
res4 <- na.omit(res4)

library(igraph)

df1 <- res4[res4$year >=2014,] %>% 
  dplyr::select(from, to) %>% 
  group_by(from, to) %>% 
  summarise(weight=n()) 
g1 <- graph_from_data_frame(df1, directed=F)
# g1 <- simplify(g1, remove.multiple = T, remove.loops = F,
#                edge.attr.comb=c(weight="sum"))

x11();plot(g1,edge.arrow.size=.5, vertex.color="gold", vertex.size=3, 
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=.5, vertex.label.dist=2, edge.curved=0.5,
     layout=layout_with_kk)

df2 <- res4[res4$year < 2014,] %>% 
  select(from, to) %>% 
  group_by(from, to) %>% 
  summarise(weight=n()) 
g2 <- graph_from_data_frame(df2, directed=F)
x11();plot(g2,edge.arrow.size=.5, vertex.color="gold", vertex.size=3, 
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=.5, vertex.label.dist=2, edge.curved=0.5,layout=layout_with_lgl)


#  Local Measures of Centrality
# Degree centrality assigns an importance score based simply 
# on the number of links held by each node.

# For finding very connected individuals, popular individuals, 
# individuals who are likely to hold most information or individuals 
# who can quickly connect with the wider network.

# Calculate degree centrality for g
g1.degree = degree(g1)
sort(g1.degree)

p1 <- data.frame(measure=g1.degree) %>% 
  rownames_to_column("country") %>% 
  slice_max(n=10, order_by=measure) %>% 
  ggplot(aes(x=reorder(country, measure), y=measure)) +
  geom_bar(stat="identity", fill=pal[2]) +
  labs(x="", y="Degree centrality") +
  coord_flip()

g2.degree = degree(g2)
sort(g2.degree)

p2 <- data.frame(measure=g2.degree) %>% 
  rownames_to_column("country") %>% 
  slice_max(n=10, order_by=measure) %>% 
  ggplot(aes(x=reorder(country, measure), y=measure)) +
  geom_bar(stat="identity", fill=pal[1]) +
  labs(x="", y="Degree centrality") +
  coord_flip()

# Global measures of centrality
# Betweenness centrality measures the number of times a node 
# lies on the shortest path between other nodes.

# For finding the individuals who influence the flow around a system.
# 
# A bit more detail: Betweenness is useful for analyzing communication dynamics, 
# but should be used with care. A high betweenness count could indicate someone holds authority over disparate clusters in a network, 
# or just that they are on the periphery of both clusters.

# Calculate betweenness centrality
g1.betweenness = betweenness(g1)
sort(g1.betweenness)

p3 <- data.frame(measure=g1.betweenness) %>% 
  rownames_to_column("country") %>% 
  slice_max(n=10, order_by=measure) %>% 
  ggplot(aes(x=reorder(country, measure), y=measure)) +
  geom_bar(stat="identity", fill=pal[2]) +
  labs(x="", y="Betweenness centrality") +
  coord_flip()

g2.betweenness = betweenness(g2)
sort(g2.betweenness)

p4 <- data.frame(measure=g2.betweenness) %>% 
  rownames_to_column("country") %>% 
  slice_max(n=10, order_by=measure) %>% 
  ggplot(aes(x=reorder(country, measure), y=measure)) +
  geom_bar(stat="identity", fill=pal[1]) +
  labs(x="", y="Betweenness centrality") +
  coord_flip()


# Like degree centrality, EigenCentrality measures a node’s influence based on the 
# number of links it has to other nodes in the network. EigenCentrality then goes a 
# step further by also taking into account how well connected a node is, and how 
# many links their connections have, and so on through the network.
# Calculate eigenvector centrality (we will need to do this for an undirected network)
g1.evcent = evcent(g1)
sort(g1.evcent$vector)

p5 <- data.frame(measure=g1.evcent$vector) %>% 
  rownames_to_column("country") %>% 
  slice_max(n=10, order_by=measure) %>% 
  ggplot(aes(x=reorder(country, measure), y=measure)) +
  geom_bar(stat="identity", fill=pal[2]) +
  labs(x="", y="Eigenvalue centrality") +
  coord_flip()

g2.evcent = evcent(g2)
sort(g2.evcent$vector)

p6 <- data.frame(measure=g2.evcent$vector) %>% 
  rownames_to_column("country") %>% 
  slice_max(n=10, order_by=measure) %>% 
  ggplot(aes(x=reorder(country, measure), y=measure)) +
  geom_bar(stat="identity", fill=pal[1]) +
  labs(x="", y="Eigenvalue centrality") +
  coord_flip()

png("plots/Supplement/Fig_S4.png", w=8, h=4, res=300, units="in")
p2+p4+p6 +  p1+p3+p5 +
  plot_layout(ncol=3) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") & 
  theme(axis.title=element_text(size=8),
        axis.text=element_text(size=8))
dev.off()
