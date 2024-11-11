# test

####created by yaoxin
### based on Marvel network data
### updated on 8/16/2020

rm(list=ls()) 
dev.off()

############network preparation#########
  
library("igraph")
data_path = 'D:/Dropbox/STUDY/02 Social network data/codes/datasets'
setwd(data_path)

#read in data
nodes <- read.csv("marvel-unimodal-nodes.csv", header=T, as.is=T)
links <- read.csv("marvel-unimodal-edges.csv", header=T, as.is=T)

# Examine the data:
head(nodes)
head(links)

# create network
net3 <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
is_directed(net3)

# remove direction of net 3
net3<-as.undirected(net3, "collapse")

############explore the network#########
#nodes and edges
gorder(net3)
gsize(net3)

# 1: degree
deg3 <- degree (net3)
summary (deg3)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max
#8.0    28.0    41.0    60.5    73.5   258.0 
hist(deg3,xlab="Degree", ylab="Frequency", 
     main = "Histogram of degrees")

##degree distribution
degree_net3 <- degree_distribution(net3, cumulative=T, mode="all")
plot( x=0:max(degree(net3)), y=1-degree_net3, pch=19, cex=0.6, col="navyblue", 
      xlab="Degree", ylab="Cumulative Frequency")

#find the largest degree
net_largenode<-V(net3)[deg3==258]
#captain America

## how many has the smallest degree
V(net3)[deg3==8]
##Asp Ii / Cleo          Black Mamba / Tanya Se


## 2: weight
hist(links$Weight, main = "")
summary(links$Weight) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.00    7.00   11.00   21.92   21.00  744.00 

links_max<-links[which(links$Weight==744),]
# The Thing / Ben Grimm and The Human Torch / Johnny Storm

links_min<-links[which(links$Weight==5),]
# 1375 obs


#3: distance and centrality
#calculate closeness
order (closeness(net3), decreasing = TRUE)
order (betweenness(net3), decreasing = TRUE)


net_close<- net3
net_close$closeness3<-closeness(net_close, weights = NULL)
summary(net_close$closeness3)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.001232 0.001531 0.001621 0.001659 0.001723 0.002538 
V(net_close)[net_close$closeness3>0.002537]
#Captain America 

dist.from.CA <- distances(net3, v=V(net3)[degree(net3)==258], 
                          to=V(net3)[degree(net3)!=258], weights=NA)
dist<-dist.from.CA[1,]
summary(dist)
#mean distance of CA to others: 1.209

net_close$betweeness3<-betweenness(net_close, weights = NULL)
betweeness3<-betweenness(net_close, weights = NULL)
summary(betweeness3)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000    4.372   18.693  142.300   67.896 3555.902 
V(net_close)[net_close$betweeness3>3555]
#Captain America


#4: CONNECTED  component
component3 <- components(net3)
component3$csize
#largest connected component: 327 nodes


#5: connectivity
#eccentricity
ecc3<-eccentricity(net3, vids = V(net3), mode = "all")
summary(ecc3) #max:3
#diameter
diameter(net3, unconnected = TRUE, weights = NULL)  #3
##node connectivity
a<-vertex_connectivity(net3, source = NULL, target = NULL,checks = TRUE)
a<-cohesion(net3, checks = TRUE)

#edge connectivity:
edge_connectivity(net3, source = NULL, target = NULL, checks = TRUE)


######plotting the network##########

##changing some parameters of nodes and edges
V(net3)$label <- NA 
V(net3)$size <- deg3 * 0.01
V(net3)$color="mediumblue"
V(net3)$frame.color=NA

E(net3)$arrow.size <- .02
E(net3)$width <- 0.1

#changing layout 
ws<-links$Weight
lay <- layout_with_fr(net3, weights=ws)
plot(net3, layout=lay)

#draw a simplified plot
net3 <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
V(net3)$label <- NA 
V(net3)$size <- deg3 * 0.01
V(net3)$color="mediumblue"
V(net3)$frame.color=NA
E(net3)$arrow.size <- .02
E(net3)$width <- 0.1
#delete some edges
cut.off <- mean(links$Weight) #choose 21.92 mean as cutoff
net_cut <- delete_edges(net3, E(net3)[Weight<cut.off])
plot(net_cut, layout=lay) 

######highlighting CaptaiN America (CA)
#try to highlight 1 node with 258 degrees
net3<-as.undirected(net3, "collapse")
net_CA<-net3
net_largenode<-V(net_CA)[degree(net_CA)==258]
V(net_CA)$color <- "navyblue"
V(net_CA)[net_largenode]$color <- "red"
V(net_CA)[net_largenode]$label <- "Captain America"
V(net_CA)[net_largenode]$size <- 10
#show only his outgoing edges
CA_edge <- incident(net_CA, V(net_CA)[net_largenode], mode="all")  #258 edges
E(net_CA)$color <- NA
E(net_CA)[CA_edge]$color <- "grey"
# CA's plot
plot(net_CA, layout = lay)
# CA's connected nodes
#library("DiagrammeR")
#get_all_connected_nodes(net_CA, node = net_largenode)
n<-0
for (i in 1:327)  {
  t3<-are_adjacent(net_CA, V(net_CA)[net_largenode], V(net_CA)[i]) 
  if (t3 == TRUE) {n <- n + 1}
}
n   #258


#######cluster detection########
#library(anocva)

# community detection
cluster3 <- cluster_leading_eigen(net3)

sizes(cluster3)
#Community sizes
#1   2   3   4 
#109 108  55  55 

modularity(cluster3)
# 0.2473808
membership(cluster3)

plot(cluster3, net3,  layout=lay)

V(net3)$size = 3
plot(net3, vertex.color=membership(cluster3), layout=lay)

plot_dendrogram(cluster3, xlab= "")

###########others possible ways of visualization########

####heat map ---- not successful
netm <-  as_adjacency_matrix(net3, attr="Weight", sparse=F)
colnames(netm) <- V(net3)$Id
rownames(netm) <- V(net3)$Id

palf <- colorRampPalette(c("yellow", "red")) 
palf <-colorRampPalette(c("red", "white", "blue"))
heatmap(netm, Rowv = NA, Colv = NA, col = palf(1000000), 
        scale="none", margins=c(10,10) )

###detach
detach("package:igraph")
#detach("package:anocva")

