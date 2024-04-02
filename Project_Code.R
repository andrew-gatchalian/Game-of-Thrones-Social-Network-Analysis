# Final Project
# Social Network Analysis of Game of Thrones
  
# **Team 8**: Sindu Bhavanam, Andrew Gatchalian, Hyo Won Lee, Dimitrios Mousouroulis, Kuan-I Wu

getwd()
setwd("/Users/andrewgatchalian/Documents/UCI MSBA 24/Winter Quarter/BANA 277 Customer & Social Analytics/Project/Data")

set.seed(123)
library(dplyr)
library("igraph")




####################################
###FULL GoT SERIES ANALYSIS

nodes <- read.csv('asoiaf-all-nodes.csv', header = T)
edges <- read.csv('asoiaf-all-edges.csv', header = T)

head(nodes)
head(edges)

nrow(nodes) # we have 796 unique characters in GoT
nrow(unique(edges[,c("Source", "Target")])) # we have 2823 unique interactions

# create network
net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F) 

# plot the network
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net)$weight*0.025,
     vertex.size = 1.2,
     vertex.color="green", vertex.frame.color="green",
     vertex.label= NA,
     layout = layout.kamada.kawai, main = "Game of Thrones Network") 

# find the diameter
diameter(net, directed = F, weights = NA)
# diameter is 9
diam_net <- get_diameter(net, directed = F, weights = E(net)$weight)
diam_net

# plot network with the diameter
igraph_options(vertex.label = NA, edge.arrow.size = 0.00001, 
               vertex.label.cex = 0.01)

V(net)$color <- "green"
V(net)$color[diam_net] <- "red"
V(net)$size<-1.3
V(net)[diam_net]$size<-4

V(net)$label <- NA
V(net)$label[diam_net] <- V(net)$name[diam_net]

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net)$weight*0.025,
     vertex.color = V(net)$color, vertex.frame.color= NA,
     vertex.label= V(net)$label, vertex.label.cex = 0.8, vertex.label.color = "black",
     vertex.label.dist = 1.3,
     layout = layout.kamada.kawai, main = "Game of Thrones Network") 


# histogram degree distributions:
par(mar = c(5.1, 4.1, 4.1, 2.1))
deg <- degree(net, mode="all")
hist(deg[deg <=200],breaks=seq(0, 150, by=1), col = "green", xlab = "Degree", ylab = "Frequency", main = "Degree Distribution ")
hist(deg[deg <=50],breaks=seq(0, 50, by=1), col = "green", xlab = "Degree", ylab = "Frequency", main = "Degree Distribution ")

# log degree distribution
deg.d <- degree_distribution(net)
d <- 1:max(deg)-1
ind <- (deg.d != 0);
plot(d[ind], deg.d[ind], log = "xy", col = "red",pch = 6, xlab = "Degree", ylab = "Frequency", main = "Log Degree Distribution");

# density
edge_density(net)

# hub scores
hub_scores <- hub_score(net)$vector
head(hub_scores)
top_hub_scores <- sort(hub_scores, decreasing = TRUE)[1:5]
top_hub_scores

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net)$weight*0.025,
     vertex.color = rainbow(52), 
     vertex.size = hub_scores * 8, 
     edge.arrow.size = 0.01, vertex.label.cex = hub_scores * .5, vertex.label.color = "black", 
     layout = layout.kamada.kawai, main = "GoT Hub Scores")

### Centrality Measures

# centrality
centr_degree(net)

# Node degrees (all) - total number of edges connected to each node
degree_net <- degree(net, mode="all")
top_degree_net <- sort(degree_net, decreasing = TRUE)[1:10]
top_degree_net
# Top 5 Total Degree:
# Tyrion-Lannister (122)
# Jon-Snow (114)
# Jaime-Lannister (101)
# Cersei-Lannister (97)
# Stannis-Baratheon  (89)

top_degree_indices <- match(names(top_degree_net), names(V(net)))

V(net)$color <- "green"
V(net)$color[top_degree_indices] <- "red"
V(net)$size<-1.2
V(net)[top_degree_indices]$size<-4

V(net)$label <- NA
V(net)$label[top_degree_indices] <- V(net)$name[top_degree_indices]

# plot degree
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net)$weight*0.025,
     vertex.color = V(net)$color, vertex.frame.color= NA,
     vertex.label= V(net)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 1,
     layout = layout.kamada.kawai, main = "GoT Network (All Degree)")

# barplot degree centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_degree_net, decreasing = FALSE), las = 2, col = "green", 
        main = "Degree Centralities in GoT (Full)",
        names.arg = names(sort(top_degree_net, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Degree Centrality",
        horiz = TRUE)

# closeness, based on distances to other nodes in the graph,
# computed as the inverse of the node's average geodesic distance to other nodes in the network.
closeness_net <- closeness(net, mode = 'all')
top_closeness_net <- sort(closeness_net, decreasing = TRUE)[1:10]
top_closeness_net
# Top 5 Closeness:
# Jaime-Lannister (0.0001205982)
# Robert-Baratheon (0.0001162791)
# Stannis-Baratheon (0.0001146921)
# Theon-Greyjoy (0.0001146132)
# Jory-Cassel (0.0001141553)

top_closeness_indices <- match(names(top_closeness_net), names(V(net)))

V(net)$color <- "green"
V(net)$color[top_closeness_indices] <- "red"
V(net)$size<-1.2
V(net)[top_closeness_indices]$size<-4

V(net)$label <- NA
V(net)$label[top_closeness_indices] <- V(net)$name[top_closeness_indices]

# plot closeness
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net)$weight*0.025,
     vertex.color = V(net)$color, vertex.frame.color= NA,
     vertex.label= V(net)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.5,
     layout = layout.kamada.kawai, main = "GoT Network (Closeness)") 

# barplot closeness centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_closeness_net, decreasing = FALSE), las = 2, col = "green", 
        main = "Closeness Centralities in GoT (Full)",
        names.arg = names(sort(top_closeness_net, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Closeness Centrality",
        horiz = TRUE)

# Betweenness (centrality based on a broker position connecting others). 
# Number of geodesics that pass through the node or the edge
betweenness_net <- betweenness(net, directed='F')
top_betweenness_net <- sort(betweenness_net, decreasing = TRUE)[1:10]
top_betweenness_net
# Top 5 Betweenness:
# Jon-Snow (41698.94)
# Theon-Greyjoy (38904.51)
# Jaime-Lannister (36856.35)
# Daenerys-Targaryen (29728.50)
# Stannis-Baratheon (29325.18)

top_betweenness_indices <- match(names(top_betweenness_net), names(V(net)))

V(net)$color <- "green"
V(net)$color[top_betweenness_indices] <- "red"
V(net)$size<-1.2
V(net)[top_betweenness_indices]$size<-4

V(net)$label <- NA
V(net)$label[top_betweenness_indices] <- V(net)$name[top_betweenness_indices]

# plot betweenness
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net)$weight*0.025,
     vertex.color = V(net)$color, vertex.frame.color= NA,
     vertex.label= V(net)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.5,
     layout = layout.kamada.kawai, main = "GoT Network (Betweenness)") 

# barplot Betweenness centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_betweenness_net, decreasing = FALSE), las = 2, col = "green", 
        main = "Betweenness Centralities in GoT (Full)",
        names.arg = names(sort(top_betweenness_net, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Betweenness Centrality",
        horiz = TRUE)


# Eigenvector (centrality proportional to the sum of connection centralities). 
# Values of the first eigenvector of the graph adjacency matrix
eigen_net <- eigen_centrality(net, directed=F, weights=E(net)$weight)
top_eigen_net <- sort(eigen_net$vector, decreasing = TRUE)[1:10]
top_eigen_net
# Top 5 Eigenvector:
# Tyrion-Lannister  (1.0000000)
# Cersei-Lannister (0.9427884)
# Joffrey-Baratheon (0.9014008)
# Robert-Baratheon (0.7429282)
# Eddard-Stark  (0.7404528)

top_eigen_indices <- match(names(top_eigen_net), names(V(net)))

V(net)$color <- "green"
V(net)$color[top_eigen_indices] <- "red"
V(net)$size<-1.2
V(net)[top_eigen_indices]$size<-4

V(net)$label <- NA
V(net)$label[top_eigen_indices] <- V(net)$name[top_eigen_indices]

# plot eigenvector
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net)$weight*0.025,
     vertex.color = V(net)$color, vertex.frame.color= NA,
     vertex.label= V(net)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.2,
     layout = layout.kamada.kawai, main = "GoT Network (Eigenvector)") 

# barplot eigenvector
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_eigen_net, decreasing = FALSE), las = 2, col = "green", 
        main = "Eigenvector Centralities in GoT (Full)",
        names.arg = names(sort(top_eigen_net, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Eigenvector Centrality",
        horiz = TRUE)

# plot all 4 centrality measures:
par(mfrow = c(2, 2), mar = c(4.1, 7.1, 2.1, 4.1))

# barplot degree centralities
barplot(sort(top_degree_net, decreasing = FALSE), las = 2, col = "green", 
        main = "Degree GoT (Full)",
        names.arg = names(sort(top_degree_net, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot closeness centralities
barplot(sort(top_closeness_net, decreasing = FALSE), las = 2, col = "green", 
        main = "Closeness GoT (Full)",
        names.arg = names(sort(top_closeness_net, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot Betweenness centralities
barplot(sort(top_betweenness_net, decreasing = FALSE), las = 2, col = "green", 
        main = "Betweenness GoT (Full)",
        names.arg = names(sort(top_betweenness_net, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot eigenvector
barplot(sort(top_eigen_net, decreasing = FALSE), las = 2, col = "green", 
        main = "Eigenvector GoT (Full)",
        names.arg = names(sort(top_eigen_net, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
par(mfrow = c(1, 1), mar = c(5.1, 8.1, 5.1, 5.1)) # reset plot

# find the most central characters in GoT
centrality_measures_full <- list(top_closeness_indices, 
                               top_betweenness_indices, 
                               top_eigen_indices, 
                               top_degree_indices)
common_indices_full <- Reduce(intersect, centrality_measures_full)

# most central characters ALL of GoT: 
cc_full <- print(V(net)[common_indices_full])
# Jaime-Lannister  Tyrion-Lannister Cersei-Lannister Jon-Snow 



####################################
### GoT BOOK (1-5) ANALYSIS

### Book 1
nodes_book1 <- read.csv('asoiaf-book1-nodes.csv', header = T)
edges_book1 <- read.csv('asoiaf-book1-edges.csv', header = T)

head(nodes_book1)
head(edges_book1)

nrow(nodes_book1) # we have 187 unique characters in book 1
nrow(unique(edges_book1[,c("Source", "Target")])) # we have 684 unique interactions

# create network
net_book1 <- graph_from_data_frame(d=edges_book1, vertices=nodes_book1, directed=F) 

# plot the network
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book1, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book1)$weight*0.025,
     vertex.size = 2.5,
     vertex.color="green", vertex.frame.color="green",
     vertex.label= NA,
     layout = layout.kamada.kawai, main = "Game of Thrones Book 1") 

# find the diameter
diameter(net_book1, directed = F, weights = NA)
# diameter is 7
diam_net_book1 <- get_diameter(net_book1, directed = F, weights = E(net_book1)$weight)
diam_net_book1

# plot network with the diameter
igraph_options(vertex.label = NA, edge.arrow.size = 0.00001, 
               vertex.label.cex = 0.01)

V(net_book1)$color <- "green"
V(net_book1)$color[diam_net_book1] <- "red"
V(net_book1)$size<-2
V(net_book1)[diam_net_book1]$size<-4

V(net_book1)$label <- NA
V(net_book1)$label[diam_net_book1] <- V(net_book1)$name[diam_net_book1]

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book1, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book1)$weight*0.025,
     vertex.color = V(net_book1)$color, vertex.frame.color= NA,
     vertex.label= V(net_book1)$label, vertex.label.cex = 0.6, vertex.label.color = "black",
     vertex.label.dist = 1,
     layout = layout.kamada.kawai, main = "GoT Book 1 Diameter") 


# histogram degree distributions:
par(mar = c(5.1, 4.1, 4.1, 2.1))
deg <- degree(net_book1, mode="all")
hist(deg[deg <=200],breaks=seq(0, 150, by=1), col = "green", xlab = "Degree", ylab = "Frequency", main = "Degree Distribution B1")
hist(deg[deg <=50],breaks=seq(0, 50, by=1), col = "green", xlab = "Degree", ylab = "Frequency", main = "Degree Distribution B1")

# log degree distribution
deg.d <- degree_distribution(net_book1)
d <- 1:max(deg)-1
ind <- (deg.d != 0);
plot(d[ind], deg.d[ind], log = "xy", col = "red",pch = 6, xlab = "Degree", ylab = "Frequency", main = "Log Degree Distribution B1");

# density
edge_density(net_book1)

# hub scores
hub_scores_b1 <- hub_score(net_book1)$vector
head(hub_scores_b1)
top_hub_scores_b1 <- sort(hub_scores_b1, decreasing = TRUE)[1:5]
top_hub_scores_b1

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book1, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book1)$weight*0.025,
     vertex.color = rainbow(52), 
     vertex.size = hub_scores_b1 * 8, 
     edge.arrow.size = 0.01, vertex.label.cex = hub_scores_b1 * .5, vertex.label.color = "black", 
     layout = layout.kamada.kawai, main = "GoT Hub Scores Book 1")


### Centrality Measures

# centrality
centr_degree(net_book1)

# Node degrees (all) - total number of edges connected to each node
degree_net_b1 <- degree(net_book1, mode="all")
top_degree_net_b1 <- sort(degree_net_b1, decreasing = TRUE)[1:10]
top_degree_net_b1
# Top 5 Total Degree (Book 1):
# Eddard-Stark   (66)
# Robert-Baratheon (50)
# Tyrion-Lannister (46)
# Catelyn-Stark (43)
# Jon-Snow  (37)

top_degree_indices_b1 <- match(names(top_degree_net_b1), names(V(net_book1)))

V(net_book1)$color <- "green"
V(net_book1)$color[top_degree_indices_b1] <- "red"
V(net_book1)$size<-1.2
V(net_book1)[top_degree_indices_b1]$size<-4

V(net_book1)$label <- NA
V(net_book1)$label[top_degree_indices_b1] <- V(net_book1)$name[top_degree_indices_b1]

# plot degree centralities
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book1, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book1)$weight*0.025,
     vertex.color = V(net_book1)$color, vertex.frame.color= NA,
     vertex.label= V(net_book1)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 1,
     layout = layout.kamada.kawai, main = "GoT Book 1 (All Degree)") 

# barplot degree centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_degree_net_b1, decreasing = FALSE), las = 2, col = "green", 
        main = "Degree Centralities in GoT Book 1",
        names.arg = names(sort(top_degree_net_b1, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Degree Centrality",
        horiz = TRUE)


# closeness, based on distances to other nodes in the graph,
# computed as the inverse of the node's average geodesic distance to other nodes in the network.
closeness_net_b1 <- closeness(net_book1, mode = 'all')
top_closeness_net_b1 <- sort(closeness_net_b1, decreasing = TRUE)[1:10]
top_closeness_net_b1
# Top 5 Closeness (Book 1):
# Robert-Baratheon (0.0005518764)
# Tyrion-Lannister (0.0005379236)
# Jaime-Lannister (0.0005367687)
# Loras-Tyrell (0.0005232862)
# Eddard-Stark  (0.0005227392)

top_closeness_indices_b1 <- match(names(top_closeness_net_b1), names(V(net_book1)))

V(net_book1)$color <- "green"
V(net_book1)$color[top_closeness_indices_b1] <- "red"
V(net_book1)$size<-1.2
V(net_book1)[top_closeness_indices_b1]$size<-4

V(net_book1)$label <- NA
V(net_book1)$label[top_closeness_indices_b1] <- V(net_book1)$name[top_closeness_indices_b1]

# plot closeness
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book1, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book1)$weight*0.025,
     vertex.color = V(net_book1)$color, vertex.frame.color= NA,
     vertex.label= V(net_book1)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.5,
     layout = layout.kamada.kawai, main = "GoT Book 1 (Closeness)") 

# barplot closeness centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_closeness_net_b1, decreasing = FALSE), las = 2, col = "green", 
        main = "Closeness Centralities in GoT Book 1",
        names.arg = names(sort(top_closeness_net_b1, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Closeness Centrality",
        horiz = TRUE)

# Betweenness (centrality based on a broker position connecting others). 
# Number of geodesics that pass through the node or the edge
betweenness_net_b1 <- betweenness(net_book1, directed='F')
top_betweenness_net_b1 <- sort(betweenness_net_b1, decreasing = TRUE)[1:10]
top_betweenness_net_b1
# Top 5 Betweenness (Book 1):
# Robert-Baratheon (4015.971)
# Eddard-Stark (3217.925)
# Tyrion-Lannister (2634.296)
# Robb-Stark  (1761.825)
# Catelyn-Stark  (1749.579)

top_betweenness_indices_b1 <- match(names(top_betweenness_net_b1), names(V(net_book1)))

V(net_book1)$color <- "green"
V(net_book1)$color[top_betweenness_indices_b1] <- "red"
V(net_book1)$size<-1.2
V(net_book1)[top_betweenness_indices_b1]$size<-4

V(net_book1)$label <- NA
V(net_book1)$label[top_betweenness_indices_b1] <- V(net_book1)$name[top_betweenness_indices_b1]

# plot Betweenness
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book1, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book1)$weight*0.025,
     vertex.color = V(net_book1)$color, vertex.frame.color= NA,
     vertex.label= V(net_book1)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.5,
     layout = layout.kamada.kawai, main = "GoT Book 1 (Betweenness)") 

# barplot Betweenness centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_betweenness_net_b1, decreasing = FALSE), las = 2, col = "green", 
        main = "Betweenness Centralities in GoT Book 1",
        names.arg = names(sort(top_betweenness_net_b1, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Betweenness Centrality",
        horiz = TRUE)


# Eigenvector (centrality proportional to the sum of connection centralities). 
# Values of the first eigenvector of the graph adjacency matrix
eigen_net_b1 <- eigen_centrality(net_book1, directed=F, weights=E(net_book1)$weight)
top_eigen_net_b1 <- sort(eigen_net_b1$vector, decreasing = TRUE)[1:10]
top_eigen_net_b1
# Top 5 Eigenvector:
# Eddard-Stark  (1.0000000)
# Robert-Baratheon (0.9158179)
# Cersei-Lannister (0.4234363)
# Sansa-Stark (0.3314489)
# Petyr-Baelish  (0.3256606)

top_eigen_indices_b1 <- match(names(top_eigen_net_b1), names(V(net_book1)))

V(net_book1)$color <- "green"
V(net_book1)$color[top_eigen_indices_b1] <- "red"
V(net_book1)$size<-1.2
V(net_book1)[top_eigen_indices_b1]$size<-4

V(net_book1)$label <- NA
V(net_book1)$label[top_eigen_indices_b1] <- V(net_book1)$name[top_eigen_indices_b1]

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book1, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book1)$weight*0.025,
     vertex.color = V(net_book1)$color, vertex.frame.color= NA,
     vertex.label= V(net_book1)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.2,
     layout = layout.kamada.kawai, main = "GoT Book 1 (Eigenvector)") 

# barplot eigenvector
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_eigen_net_b1, decreasing = FALSE), las = 2, col = "green", 
        main = "Eigenvector Centralities in GoT Book 1",
        names.arg = names(sort(top_eigen_net_b1, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Eigenvector Centrality",
        horiz = TRUE)


# plot all 4 centrality measures:
par(mfrow = c(2, 2), mar = c(4.1, 7.1, 2.1, 4.1))

# barplot degree centralities
barplot(sort(top_degree_net_b1, decreasing = FALSE), las = 2, col = "green", 
        main = "Degree GoT Book 1",
        names.arg = names(sort(top_degree_net_b1, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot closeness centralities
barplot(sort(top_closeness_net_b1, decreasing = FALSE), las = 2, col = "green", 
        main = "Closeness GoT Book 1",
        names.arg = names(sort(top_closeness_net_b1, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot Betweenness centralities
barplot(sort(top_betweenness_net_b1, decreasing = FALSE), las = 2, col = "green", 
        main = "Betweenness GoT Book 1",
        names.arg = names(sort(top_betweenness_net_b1, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot eigenvector
barplot(sort(top_eigen_net_b1, decreasing = FALSE), las = 2, col = "green", 
        main = "Eigenvector GoT Book 1",
        names.arg = names(sort(top_eigen_net_b1, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
par(mfrow = c(1, 1), mar = c(5.1, 8.1, 5.1, 5.1)) # reset plot

# find the most central characters in book 1
centrality_measures_b1 <- list(top_closeness_indices_b1, 
                               top_betweenness_indices_b1, 
                               top_eigen_indices_b1, 
                               top_degree_indices_b1)
common_indices_b1 <- Reduce(intersect, centrality_measures_b1)

# most central characters book 1: 
cc_book1 <- print(V(net_book1)[common_indices_b1])
# Robert-Baratheon Tyrion-Lannister Eddard-Stark




### Book 2
nodes_book2 <- read.csv('asoiaf-book2-nodes.csv', header = T)
edges_book2 <- read.csv('asoiaf-book2-edges.csv', header = T)

nrow(nodes_book2) # we have 259 unique characters in book 2
nrow(unique(edges_book2[,c("Source", "Target")])) # we have 775 unique interactions

# create network
net_book2 <- graph_from_data_frame(d=edges_book2, vertices=nodes_book2, directed=F) 

# plot the network
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book2, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book2)$weight*0.025,
     vertex.size = 2.5,
     vertex.color="green", vertex.frame.color="green",
     vertex.label= NA,
     layout = layout.kamada.kawai, main = "Game of Thrones Book 2") 

# find the diameter
diameter(net_book2, directed = F, weights = NA)
# diameter is 8
diam_net_book2 <- get_diameter(net_book2, directed = F, weights = E(net_book2)$weight)
diam_net_book2

# plot network with the diameter
igraph_options(vertex.label = NA, edge.arrow.size = 0.00001, 
               vertex.label.cex = 0.01)

V(net_book2)$color <- "green"
V(net_book2)$color[diam_net_book2] <- "red"
V(net_book2)$size<-2
V(net_book2)[diam_net_book2]$size<-4

V(net_book2)$label <- NA
V(net_book2)$label[diam_net_book2] <- V(net_book2)$name[diam_net_book2]

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book2, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book2)$weight*0.025,
     vertex.color = V(net_book2)$color, vertex.frame.color= NA,
     vertex.label= V(net_book2)$label, vertex.label.cex = 0.6, vertex.label.color = "black",
     vertex.label.dist = 1,
     layout = layout.kamada.kawai, main = "GoT Book 2 Diameter") 


# histogram degree distributions:
par(mar = c(5.1, 4.1, 4.1, 2.1))
deg <- degree(net_book2, mode="all")
hist(deg[deg <=200],breaks=seq(0, 150, by=1), col = "green", xlab = "Degree", ylab = "Frequency", main = "Degree Distribution B2")
hist(deg[deg <=50],breaks=seq(0, 50, by=1), col = "green", xlab = "Degree", ylab = "Frequency", main = "Degree Distribution B2")

# log degree distribution
deg.d <- degree_distribution(net_book2)
d <- 1:max(deg)-1
ind <- (deg.d != 0);
plot(d[ind], deg.d[ind], log = "xy", col = "red",pch = 6, xlab = "Degree", ylab = "Frequency", main = "Log Degree Distribution B2");

# density
edge_density(net_book2)

# hub scores
hub_scores_b2 <- hub_score(net_book2)$vector
head(hub_scores_b2)
top_hub_scores_b2 <- sort(hub_scores_b2, decreasing = TRUE)[1:5]
top_hub_scores_b2

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book2, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book2)$weight*0.025,
     vertex.color = rainbow(52), 
     vertex.size = hub_scores_b2 * 8, 
     edge.arrow.size = 0.01, vertex.label.cex = hub_scores_b2 * .5, vertex.label.color = "black", 
     layout = layout.kamada.kawai, main = "GoT Hub Scores Book 2")


### Centrality Measures

# centrality
centr_degree(net_book2)

# Node degrees (all) - total number of edges connected to each node
degree_net_b2 <- degree(net_book2, mode="all")
top_degree_net_b2 <- sort(degree_net_b2, decreasing = TRUE)[1:10]
top_degree_net_b2

top_degree_indices_b2 <- match(names(top_degree_net_b2), names(V(net_book2)))

V(net_book2)$color <- "green"
V(net_book2)$color[top_degree_indices_b2] <- "red"
V(net_book2)$size<-1.2
V(net_book2)[top_degree_indices_b2]$size<-4

V(net_book2)$label <- NA
V(net_book2)$label[top_degree_indices_b2] <- V(net_book2)$name[top_degree_indices_b2]

# plot degree centralities
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book2, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book2)$weight*0.025,
     vertex.color = V(net_book2)$color, vertex.frame.color= NA,
     vertex.label= V(net_book2)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 1,
     layout = layout.kamada.kawai, main = "GoT Book 2 (All Degree)") 

# barplot degree centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_degree_net_b2, decreasing = FALSE), las = 2, col = "green", 
        main = "Degree Centralities in GoT Book 2",
        names.arg = names(sort(top_degree_net_b2, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Degree Centrality",
        horiz = TRUE)


# closeness, based on distances to other nodes in the graph,
# computed as the inverse of the node's average geodesic distance to other nodes in the network.
closeness_net_b2 <- closeness(net_book2, mode = 'all')
top_closeness_net_b2 <- sort(closeness_net_b2, decreasing = TRUE)[1:10]
top_closeness_net_b2

top_closeness_indices_b2 <- match(names(top_closeness_net_b2), names(V(net_book2)))

V(net_book2)$color <- "green"
V(net_book2)$color[top_closeness_indices_b2] <- "red"
V(net_book2)$size<-1.2
V(net_book2)[top_closeness_indices_b2]$size<-4

V(net_book2)$label <- NA
V(net_book2)$label[top_closeness_indices_b2] <- V(net_book2)$name[top_closeness_indices_b2]

# plot closeness
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book2, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book2)$weight*0.025,
     vertex.color = V(net_book2)$color, vertex.frame.color= NA,
     vertex.label= V(net_book2)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.5,
     layout = layout.kamada.kawai, main = "GoT Book 2 (Closeness)") 

# barplot closeness centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_closeness_net_b2, decreasing = FALSE), las = 2, col = "green", 
        main = "Closeness Centralities in GoT Book 2",
        names.arg = names(sort(top_closeness_net_b2, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)

# Betweenness (centrality based on a broker position connecting others). 
# Number of geodesics that pass through the node or the edge
betweenness_net_b2 <- betweenness(net_book2, directed='F')
top_betweenness_net_b2 <- sort(betweenness_net_b2, decreasing = TRUE)[1:10]
top_betweenness_net_b2

top_betweenness_indices_b2 <- match(names(top_betweenness_net_b2), names(V(net_book2)))

V(net_book2)$color <- "green"
V(net_book2)$color[top_betweenness_indices_b2] <- "red"
V(net_book2)$size<-1.2
V(net_book2)[top_betweenness_indices_b2]$size<-4

V(net_book2)$label <- NA
V(net_book2)$label[top_betweenness_indices_b2] <- V(net_book2)$name[top_betweenness_indices_b2]

# plot Betweenness
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book2, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book2)$weight*0.025,
     vertex.color = V(net_book2)$color, vertex.frame.color= NA,
     vertex.label= V(net_book2)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.5,
     layout = layout.kamada.kawai, main = "GoT Book 2 (Betweenness)") 

# barplot Betweenness centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_betweenness_net_b2, decreasing = FALSE), las = 2, col = "green", 
        main = "Betweenness Centralities in GoT Book 2",
        names.arg = names(sort(top_betweenness_net_b2, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Betweenness Centrality",
        horiz = TRUE)

# Eigenvector (centrality proportional to the sum of connection centralities). 
# Values of the first eigenvector of the graph adjacency matrix
eigen_net_b2 <- eigen_centrality(net_book2, directed=F, weights=E(net_book2)$weight)
top_eigen_net_b2 <- sort(eigen_net_b2$vector, decreasing = TRUE)[1:10]
top_eigen_net_b2

top_eigen_indices_b2 <- match(names(top_eigen_net_b2), names(V(net_book2)))

V(net_book2)$color <- "green"
V(net_book2)$color[top_eigen_indices_b2] <- "red"
V(net_book2)$size<-1.2
V(net_book2)[top_eigen_indices_b2]$size<-4

V(net_book2)$label <- NA
V(net_book2)$label[top_eigen_indices_b2] <- V(net_book2)$name[top_eigen_indices_b2]

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book2, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book2)$weight*0.025,
     vertex.color = V(net_book2)$color, vertex.frame.color= NA,
     vertex.label= V(net_book2)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.2,
     layout = layout.kamada.kawai, main = "GoT Book 2 (Eigenvector)") 

# barplot eigenvector
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_eigen_net_b2, decreasing = FALSE), las = 2, col = "green", 
        main = "Eigenvector Centralities in GoT Book 2",
        names.arg = names(sort(top_eigen_net_b2, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Eigenvector Centrality",
        horiz = TRUE)


# plot all 4 centrality measures:
par(mfrow = c(2, 2), mar = c(4.1, 7.1, 2.1, 4.1))

# barplot degree centralities
barplot(sort(top_degree_net_b2, decreasing = FALSE), las = 2, col = "green", 
        main = "Degree GoT Book 2",
        names.arg = names(sort(top_degree_net_b2, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot closeness centralities
barplot(sort(top_closeness_net_b2, decreasing = FALSE), las = 2, col = "green", 
        main = "Closeness GoT Book 2",
        names.arg = names(sort(top_closeness_net_b2, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot Betweenness centralities
barplot(sort(top_betweenness_net_b2, decreasing = FALSE), las = 2, col = "green", 
        main = "Betweenness GoT Book 2",
        names.arg = names(sort(top_betweenness_net_b2, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot eigenvector
barplot(sort(top_eigen_net_b2, decreasing = FALSE), las = 2, col = "green", 
        main = "Eigenvector GoT Book 2",
        names.arg = names(sort(top_eigen_net_b2, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
par(mfrow = c(1, 1), mar = c(5.1, 8.1, 5.1, 5.1)) # reset plot

# find the most central characters in book 2
centrality_measures_b2 <- list(top_closeness_indices_b2, 
                               top_betweenness_indices_b2, 
                               top_eigen_indices_b2, 
                               top_degree_indices_b2)
common_indices_b2 <- Reduce(intersect, centrality_measures_b2)

# most central characters book 2: 
cc_book2 <- print(V(net_book2)[common_indices_b2])
# Catelyn-Stark Joffrey-Baratheon Robb-Stark




### Book 3
nodes_book3 <- read.csv('asoiaf-book3-nodes.csv', header = T)
edges_book3 <- read.csv('asoiaf-book3-edges.csv', header = T)

nrow(nodes_book3) # we have 303 unique characters in book 3
nrow(unique(edges_book3[,c("Source", "Target")])) # we have 1008 unique interactions

# create network
net_book3 <- graph_from_data_frame(d=edges_book3, vertices=nodes_book3, directed=F) 

# plot the network
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book3, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book3)$weight*0.025,
     vertex.size = 2,
     vertex.color="green", vertex.frame.color="green",
     vertex.label= NA,
     layout = layout.kamada.kawai, main = "Game of Thrones Book 3") 

# find the diameter
diameter(net_book3, directed = F, weights = NA)
# diameter is 8
diam_net_book3 <- get_diameter(net_book3, directed = F, weights = E(net_book3)$weight)
diam_net_book3

# plot network with the diameter
igraph_options(vertex.label = NA, edge.arrow.size = 0.00001, 
               vertex.label.cex = 0.01)

V(net_book3)$color <- "green"
V(net_book3)$color[diam_net_book3] <- "red"
V(net_book3)$size<-2
V(net_book3)[diam_net_book3]$size<-4

V(net_book3)$label <- NA
V(net_book3)$label[diam_net_book3] <- V(net_book3)$name[diam_net_book3]

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book3, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book3)$weight*0.025,
     vertex.color = V(net_book3)$color, vertex.frame.color= NA,
     vertex.label= V(net_book3)$label, vertex.label.cex = 0.6, vertex.label.color = "black",
     vertex.label.dist = 1,
     layout = layout.kamada.kawai, main = "GoT Book 3 Diameter") 


# histogram degree distributions:
par(mar = c(5.1, 4.1, 4.1, 2.1))
deg <- degree(net_book3, mode="all")
hist(deg[deg <=200],breaks=seq(0, 150, by=1), col = "green", xlab = "Degree", ylab = "Frequency", main = "Degree Distribution B3")
hist(deg[deg <=50],breaks=seq(0, 50, by=1), col = "green", xlab = "Degree", ylab = "Frequency", main = "Degree Distribution B3")

# log degree distribution
deg.d <- degree_distribution(net_book3)
d <- 1:max(deg)-1
ind <- (deg.d != 0);
plot(d[ind], deg.d[ind], log = "xy", col = "red",pch = 6, xlab = "Degree", ylab = "Frequency", main = "Log Degree Distribution B3");

# density
edge_density(net_book3)

# hub scores
hub_scores_b3 <- hub_score(net_book3)$vector
head(hub_scores_b3)
top_hub_scores_b3 <- sort(hub_scores_b3, decreasing = TRUE)[1:5]
top_hub_scores_b3

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book3, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book3)$weight*0.025,
     vertex.color = rainbow(52), 
     vertex.size = hub_scores_b3 * 8, 
     edge.arrow.size = 0.01, vertex.label.cex = hub_scores_b3 * .5, vertex.label.color = "black", 
     layout = layout.kamada.kawai, main = "GoT Hub Scores Book 3")


### Centrality Measures

# centrality
centr_degree(net_book3)

# Node degrees (all) - total number of edges connected to each node
degree_net_b3 <- degree(net_book3, mode="all")
top_degree_net_b3 <- sort(degree_net_b3, decreasing = TRUE)[1:10]
top_degree_net_b3

top_degree_indices_b3 <- match(names(top_degree_net_b3), names(V(net_book3)))

V(net_book3)$color <- "green"
V(net_book3)$color[top_degree_indices_b3] <- "red"
V(net_book3)$size<-1.2
V(net_book3)[top_degree_indices_b3]$size<-4

V(net_book3)$label <- NA
V(net_book3)$label[top_degree_indices_b3] <- V(net_book3)$name[top_degree_indices_b3]

# plot degree centralities
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book3, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book3)$weight*0.025,
     vertex.color = V(net_book3)$color, vertex.frame.color= NA,
     vertex.label= V(net_book3)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 1,
     layout = layout.kamada.kawai, main = "GoT Book 3 (All Degree)") 

# barplot degree centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_degree_net_b3, decreasing = FALSE), las = 2, col = "green", 
        main = "Degree Centralities in GoT Book 3",
        names.arg = names(sort(top_degree_net_b3, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Degree Centrality",
        horiz = TRUE)


# closeness, based on distances to other nodes in the graph,
# computed as the inverse of the node's average geodesic distance to other nodes in the network.
closeness_net_b3 <- closeness(net_book3, mode = 'all')
top_closeness_net_b3 <- sort(closeness_net_b3, decreasing = TRUE)[1:10]
top_closeness_net_b3

top_closeness_indices_b3 <- match(names(top_closeness_net_b3), names(V(net_book3)))

V(net_book3)$color <- "green"
V(net_book3)$color[top_closeness_indices_b3] <- "red"
V(net_book3)$size<-1.2
V(net_book3)[top_closeness_indices_b3]$size<-4

V(net_book3)$label <- NA
V(net_book3)$label[top_closeness_indices_b3] <- V(net_book3)$name[top_closeness_indices_b3]

# plot closeness
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book3, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book3)$weight*0.025,
     vertex.color = V(net_book3)$color, vertex.frame.color= NA,
     vertex.label= V(net_book3)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.5,
     layout = layout.kamada.kawai, main = "GoT Book 3 (Closeness)") 

# barplot closeness centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_closeness_net_b3, decreasing = FALSE), las = 2, col = "green", 
        main = "Closeness Centralities in GoT Book 3",
        names.arg = names(sort(top_closeness_net_b3, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)

# Betweenness (centrality based on a broker position connecting others). 
# Number of geodesics that pass through the node or the edge
betweenness_net_b3 <- betweenness(net_book3, directed='F')
top_betweenness_net_b3 <- sort(betweenness_net_b3, decreasing = TRUE)[1:10]
top_betweenness_net_b3

top_betweenness_indices_b3 <- match(names(top_betweenness_net_b3), names(V(net_book3)))

V(net_book3)$color <- "green"
V(net_book3)$color[top_betweenness_indices_b3] <- "red"
V(net_book3)$size<-1.2
V(net_book3)[top_betweenness_indices_b3]$size<-4

V(net_book3)$label <- NA
V(net_book3)$label[top_betweenness_indices_b3] <- V(net_book3)$name[top_betweenness_indices_b3]

# plot Betweenness
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book3, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book3)$weight*0.025,
     vertex.color = V(net_book3)$color, vertex.frame.color= NA,
     vertex.label= V(net_book3)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.5,
     layout = layout.kamada.kawai, main = "GoT Book 3 (Betweenness)") 

# barplot Betweenness centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_betweenness_net_b3, decreasing = FALSE), las = 2, col = "green", 
        main = "Betweenness Centralities in GoT Book 3",
        names.arg = names(sort(top_betweenness_net_b3, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Betweenness Centrality",
        horiz = TRUE)

# Eigenvector (centrality proportional to the sum of connection centralities). 
# Values of the first eigenvector of the graph adjacency matrix
eigen_net_b3 <- eigen_centrality(net_book3, directed=F, weights=E(net_book3)$weight)
top_eigen_net_b3 <- sort(eigen_net_b3$vector, decreasing = TRUE)[1:10]
top_eigen_net_b3

top_eigen_indices_b3 <- match(names(top_eigen_net_b3), names(V(net_book3)))

V(net_book3)$color <- "green"
V(net_book3)$color[top_eigen_indices_b3] <- "red"
V(net_book3)$size<-1.2
V(net_book3)[top_eigen_indices_b3]$size<-4

V(net_book3)$label <- NA
V(net_book3)$label[top_eigen_indices_b3] <- V(net_book3)$name[top_eigen_indices_b3]

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book3, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book3)$weight*0.025,
     vertex.color = V(net_book3)$color, vertex.frame.color= NA,
     vertex.label= V(net_book3)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.2,
     layout = layout.kamada.kawai, main = "GoT Book 3 (Eigenvector)") 

# barplot eigenvector
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_eigen_net_b3, decreasing = FALSE), las = 2, col = "green", 
        main = "Eigenvector Centralities in GoT Book 3",
        names.arg = names(sort(top_eigen_net_b3, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Eigenvector Centrality",
        horiz = TRUE)


# plot all 4 centrality measures:
par(mfrow = c(2, 2), mar = c(4.1, 7.1, 2.1, 4.1))

# barplot degree centralities
barplot(sort(top_degree_net_b3, decreasing = FALSE), las = 2, col = "green", 
        main = "Degree GoT Book 3",
        names.arg = names(sort(top_degree_net_b3, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot closeness centralities
barplot(sort(top_closeness_net_b3, decreasing = FALSE), las = 2, col = "green", 
        main = "Closeness GoT Book 3",
        names.arg = names(sort(top_closeness_net_b3, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot Betweenness centralities
barplot(sort(top_betweenness_net_b3, decreasing = FALSE), las = 2, col = "green", 
        main = "Betweenness GoT Book 3",
        names.arg = names(sort(top_betweenness_net_b3, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot eigenvector
barplot(sort(top_eigen_net_b3, decreasing = FALSE), las = 2, col = "green", 
        main = "Eigenvector GoT Book 3",
        names.arg = names(sort(top_eigen_net_b3, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
par(mfrow = c(1, 1), mar = c(5.1, 8.1, 5.1, 5.1)) # reset plot

# find the most central characters in book 3
centrality_measures_b3 <- list(top_closeness_indices_b3, 
                               top_betweenness_indices_b3, 
                               top_eigen_indices_b3, 
                               top_degree_indices_b3)
common_indices_b3 <- Reduce(intersect, centrality_measures_b3)

# most central characters book 3: 
cc_book3 <- print(V(net_book3)[common_indices_b3])
# Joffrey-Baratheon Tyrion-Lannister  Sansa-Stark Jaime-Lannister 





### Book 4
nodes_book4 <- read.csv('asoiaf-book4-nodes.csv', header = T)
edges_book4 <- read.csv('asoiaf-book4-edges.csv', header = T)

nrow(nodes_book4) # we have 274 unique characters in book 4
nrow(unique(edges_book4[,c("Source", "Target")])) # we have 682 unique interactions

# create network
net_book4 <- graph_from_data_frame(d=edges_book4, vertices=nodes_book4, directed=F) 

# plot the network
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book4, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book4)$weight*0.025,
     vertex.size = 2,
     vertex.color="green", vertex.frame.color="green",
     vertex.label= NA,
     layout = layout.kamada.kawai, main = "Game of Thrones Book 4") 

# find the diameter
diameter(net_book4, directed = F, weights = NA)
# diameter is 10
diam_net_book4 <- get_diameter(net_book4, directed = F, weights = E(net_book4)$weight)
diam_net_book4

# plot network with the diameter
igraph_options(vertex.label = NA, edge.arrow.size = 0.00001, 
               vertex.label.cex = 0.01)

V(net_book4)$color <- "green"
V(net_book4)$color[diam_net_book4] <- "red"
V(net_book4)$size<-2
V(net_book4)[diam_net_book4]$size<-4

V(net_book4)$label <- NA
V(net_book4)$label[diam_net_book4] <- V(net_book4)$name[diam_net_book4]

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book4, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book4)$weight*0.025,
     vertex.color = V(net_book4)$color, vertex.frame.color= NA,
     vertex.label= V(net_book4)$label, vertex.label.cex = 0.6, vertex.label.color = "black",
     vertex.label.dist = 1,
     layout = layout.kamada.kawai, main = "GoT Book 4 Diameter") 


# histogram degree distributions:
par(mar = c(5.1, 4.1, 4.1, 2.1))
deg <- degree(net_book4, mode="all")
hist(deg[deg <=200],breaks=seq(0, 150, by=1), col = "green", xlab = "Degree", ylab = "Frequency", main = "Degree Distribution B4")
hist(deg[deg <=50],breaks=seq(0, 50, by=1), col = "green", xlab = "Degree", ylab = "Frequency", main = "Degree Distribution B4")

# log degree distribution
deg.d <- degree_distribution(net_book4)
d <- 1:max(deg)-1
ind <- (deg.d != 0);
plot(d[ind], deg.d[ind], log = "xy", col = "red",pch = 6, xlab = "Degree", ylab = "Frequency", main = "Log Degree Distribution B4");

# density
edge_density(net_book4)

# hub scores
hub_scores_b4 <- hub_score(net_book4)$vector
head(hub_scores_b4)
top_hub_scores_b4 <- sort(hub_scores_b4, decreasing = TRUE)[1:5]
top_hub_scores_b4

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book4, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book4)$weight*0.025,
     vertex.color = rainbow(52), 
     vertex.size = hub_scores_b4 * 8, 
     edge.arrow.size = 0.01, vertex.label.cex = hub_scores_b4 * .5, vertex.label.color = "black", 
     layout = layout.kamada.kawai, main = "GoT Hub Scores Book 4")


### Centrality Measures

# centrality
centr_degree(net_book4)

# Node degrees (all) - total number of edges connected to each node
degree_net_b4 <- degree(net_book4, mode="all")
top_degree_net_b4 <- sort(degree_net_b4, decreasing = TRUE)[1:10]
top_degree_net_b4

top_degree_indices_b4 <- match(names(top_degree_net_b4), names(V(net_book4)))

V(net_book4)$color <- "green"
V(net_book4)$color[top_degree_indices_b4] <- "red"
V(net_book4)$size<-1.2
V(net_book4)[top_degree_indices_b4]$size<-4

V(net_book4)$label <- NA
V(net_book4)$label[top_degree_indices_b4] <- V(net_book4)$name[top_degree_indices_b4]

# plot degree centralities
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book4, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book4)$weight*0.025,
     vertex.color = V(net_book4)$color, vertex.frame.color= NA,
     vertex.label= V(net_book4)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 1,
     layout = layout.kamada.kawai, main = "GoT Book 4 (All Degree)") 

# barplot degree centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_degree_net_b4, decreasing = FALSE), las = 2, col = "green", 
        main = "Degree Centralities in GoT Book 4",
        names.arg = names(sort(top_degree_net_b4, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Degree Centrality",
        horiz = TRUE)


# closeness, based on distances to other nodes in the graph,
# computed as the inverse of the node's average geodesic distance to other nodes in the network.
closeness_net_b4 <- closeness(net_book4, mode = 'all')
top_closeness_net_b4 <- sort(closeness_net_b4, decreasing = TRUE)[1:10]
top_closeness_net_b4

top_closeness_indices_b4 <- match(names(top_closeness_net_b4), names(V(net_book4)))

V(net_book4)$color <- "green"
V(net_book4)$color[top_closeness_indices_b4] <- "red"
V(net_book4)$size<-1.2
V(net_book4)[top_closeness_indices_b4]$size<-4

V(net_book4)$label <- NA
V(net_book4)$label[top_closeness_indices_b4] <- V(net_book4)$name[top_closeness_indices_b4]

# plot closeness
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book4, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book4)$weight*0.025,
     vertex.color = V(net_book4)$color, vertex.frame.color= NA,
     vertex.label= V(net_book4)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.5,
     layout = layout.kamada.kawai, main = "GoT Book 4 (Closeness)") 

# barplot closeness centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_closeness_net_b4, decreasing = FALSE), las = 2, col = "green", 
        main = "Closeness Centralities in GoT Book 4",
        names.arg = names(sort(top_closeness_net_b4, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)

# Betweenness (centrality based on a broker position connecting others). 
# Number of geodesics that pass through the node or the edge
betweenness_net_b4 <- betweenness(net_book4, directed='F')
top_betweenness_net_b4 <- sort(betweenness_net_b4, decreasing = TRUE)[1:10]
top_betweenness_net_b4

top_betweenness_indices_b4 <- match(names(top_betweenness_net_b4), names(V(net_book4)))

V(net_book4)$color <- "green"
V(net_book4)$color[top_betweenness_indices_b4] <- "red"
V(net_book4)$size<-1.2
V(net_book4)[top_betweenness_indices_b4]$size<-4

V(net_book4)$label <- NA
V(net_book4)$label[top_betweenness_indices_b4] <- V(net_book4)$name[top_betweenness_indices_b4]

# plot Betweenness
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book4, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book4)$weight*0.025,
     vertex.color = V(net_book4)$color, vertex.frame.color= NA,
     vertex.label= V(net_book4)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.5,
     layout = layout.kamada.kawai, main = "GoT Book 4 (Betweenness)") 

# barplot Betweenness centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_betweenness_net_b4, decreasing = FALSE), las = 2, col = "green", 
        main = "Betweenness Centralities in GoT Book 4",
        names.arg = names(sort(top_betweenness_net_b4, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Betweenness Centrality",
        horiz = TRUE)

# Eigenvector (centrality proportional to the sum of connection centralities). 
# Values of the first eigenvector of the graph adjacency matrix
eigen_net_b4 <- eigen_centrality(net_book4, directed=F, weights=E(net_book4)$weight)
top_eigen_net_b4 <- sort(eigen_net_b4$vector, decreasing = TRUE)[1:10]
top_eigen_net_b4

top_eigen_indices_b4 <- match(names(top_eigen_net_b4), names(V(net_book4)))

V(net_book4)$color <- "green"
V(net_book4)$color[top_eigen_indices_b4] <- "red"
V(net_book4)$size<-1.2
V(net_book4)[top_eigen_indices_b4]$size<-4

V(net_book4)$label <- NA
V(net_book4)$label[top_eigen_indices_b4] <- V(net_book4)$name[top_eigen_indices_b4]

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book4, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book4)$weight*0.025,
     vertex.color = V(net_book4)$color, vertex.frame.color= NA,
     vertex.label= V(net_book4)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.2,
     layout = layout.kamada.kawai, main = "GoT Book 4 (Eigenvector)") 

# barplot eigenvector
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_eigen_net_b4, decreasing = FALSE), las = 2, col = "green", 
        main = "Eigenvector Centralities in GoT Book 4",
        names.arg = names(sort(top_eigen_net_b4, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Eigenvector Centrality",
        horiz = TRUE)


# plot all 4 centrality measures:
par(mfrow = c(2, 2), mar = c(4.1, 7.1, 2.1, 4.1))

# barplot degree centralities
barplot(sort(top_degree_net_b4, decreasing = FALSE), las = 2, col = "green", 
        main = "Degree GoT Book 4",
        names.arg = names(sort(top_degree_net_b4, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot closeness centralities
barplot(sort(top_closeness_net_b4, decreasing = FALSE), las = 2, col = "green", 
        main = "Closeness GoT Book 4",
        names.arg = names(sort(top_closeness_net_b4, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot Betweenness centralities
barplot(sort(top_betweenness_net_b4, decreasing = FALSE), las = 2, col = "green", 
        main = "Betweenness GoT Book 4",
        names.arg = names(sort(top_betweenness_net_b4, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot eigenvector
barplot(sort(top_eigen_net_b4, decreasing = FALSE), las = 2, col = "green", 
        main = "Eigenvector GoT Book 4",
        names.arg = names(sort(top_eigen_net_b4, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
par(mfrow = c(1, 1), mar = c(5.1, 8.1, 5.1, 5.1)) # reset plot

# find the most central characters in book 4
centrality_measures_b4 <- list(top_closeness_indices_b4, 
                               top_betweenness_indices_b4, 
                               top_eigen_indices_b4, 
                               top_degree_indices_b4)
common_indices_b4 <- Reduce(intersect, centrality_measures_b4)

# most central characters book 4: 
cc_book4 <- print(V(net_book4)[common_indices_b4])
# Tyrion-Lannister Cersei-Lannister Jaime-Lannister 






### Book 5
nodes_book5 <- read.csv('asoiaf-book5-nodes.csv', header = T)
edges_book5 <- read.csv('asoiaf-book5-edges.csv', header = T)

nrow(nodes_book5) # we have 317 unique characters in book 5
nrow(unique(edges_book5[,c("Source", "Target")])) # we have 760 unique interactions

# create network
net_book5 <- graph_from_data_frame(d=edges_book5, vertices=nodes_book5, directed=F) 

# plot the network
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book5, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book5)$weight*0.025,
     vertex.size = 2,
     vertex.color="green", vertex.frame.color="green",
     vertex.label= NA,
     layout = layout.kamada.kawai, main = "Game of Thrones Book 5") 

# find the diameter
diameter(net_book5, directed = F, weights = NA)
# diameter is 8
diam_net_book5 <- get_diameter(net_book5, directed = F, weights = E(net_book5)$weight)
diam_net_book5

# plot network with the diameter
igraph_options(vertex.label = NA, edge.arrow.size = 0.00001, 
               vertex.label.cex = 0.01)

V(net_book5)$color <- "green"
V(net_book5)$color[diam_net_book5] <- "red"
V(net_book5)$size<-2
V(net_book5)[diam_net_book5]$size<-4

V(net_book5)$label <- NA
V(net_book5)$label[diam_net_book5] <- V(net_book5)$name[diam_net_book5]

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book5, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book5)$weight*0.025,
     vertex.color = V(net_book5)$color, vertex.frame.color= NA,
     vertex.label= V(net_book5)$label, vertex.label.cex = 0.6, vertex.label.color = "black",
     vertex.label.dist = 1,
     layout = layout.kamada.kawai, main = "GoT Book 5 Diameter") 


# histogram degree distributions:
par(mar = c(5.1, 4.1, 4.1, 2.1))
deg <- degree(net_book5, mode="all")
hist(deg[deg <=200],breaks=seq(0, 150, by=1), col = "green", xlab = "Degree", ylab = "Frequency", main = "Degree Distribution B5")
hist(deg[deg <=50],breaks=seq(0, 50, by=1), col = "green", xlab = "Degree", ylab = "Frequency", main = "Degree Distribution B5")

# log degree distribution
deg.d <- degree_distribution(net_book5)
d <- 1:max(deg)-1
ind <- (deg.d != 0);
plot(d[ind], deg.d[ind], log = "xy", col = "red",pch = 6, xlab = "Degree", ylab = "Frequency", main = "Log Degree Distribution B5");

# density
edge_density(net_book5)

# hub scores
hub_scores_b5 <- hub_score(net_book5)$vector
head(hub_scores_b5)
top_hub_scores_b5 <- sort(hub_scores_b5, decreasing = TRUE)[1:5]
top_hub_scores_b5

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book5, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book5)$weight*0.025,
     vertex.color = rainbow(52), 
     vertex.size = hub_scores_b5 * 8, 
     edge.arrow.size = 0.01, vertex.label.cex = hub_scores_b5 * .5, vertex.label.color = "black", 
     layout = layout.kamada.kawai, main = "GoT Hub Scores Book 5")


### Centrality Measures

# centrality
centr_degree(net_book5)

# Node degrees (all) - total number of edges connected to each node
degree_net_b5 <- degree(net_book5, mode="all")
top_degree_net_b5 <- sort(degree_net_b5, decreasing = TRUE)[1:10]
top_degree_net_b5

top_degree_indices_b5 <- match(names(top_degree_net_b5), names(V(net_book5)))

V(net_book5)$color <- "green"
V(net_book5)$color[top_degree_indices_b5] <- "red"
V(net_book5)$size<-1.2
V(net_book5)[top_degree_indices_b5]$size<-4

V(net_book5)$label <- NA
V(net_book5)$label[top_degree_indices_b5] <- V(net_book5)$name[top_degree_indices_b5]

# plot degree centralities
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book5, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book5)$weight*0.025,
     vertex.color = V(net_book5)$color, vertex.frame.color= NA,
     vertex.label= V(net_book5)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 1,
     layout = layout.kamada.kawai, main = "GoT Book 5 (All Degree)") 

# barplot degree centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_degree_net_b5, decreasing = FALSE), las = 2, col = "green", 
        main = "Degree Centralities in GoT Book 5",
        names.arg = names(sort(top_degree_net_b5, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Degree Centrality",
        horiz = TRUE)


# closeness, based on distances to other nodes in the graph,
# computed as the inverse of the node's average geodesic distance to other nodes in the network.
closeness_net_b5 <- closeness(net_book5, mode = 'all')
top_closeness_net_b5 <- sort(closeness_net_b5, decreasing = TRUE)[1:10]
top_closeness_net_b5

top_closeness_indices_b5 <- match(names(top_closeness_net_b5), names(V(net_book5)))

V(net_book5)$color <- "green"
V(net_book5)$color[top_closeness_indices_b5] <- "red"
V(net_book5)$size<-1.2
V(net_book5)[top_closeness_indices_b5]$size<-4

V(net_book5)$label <- NA
V(net_book5)$label[top_closeness_indices_b5] <- V(net_book5)$name[top_closeness_indices_b5]

# plot closeness
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book5, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book5)$weight*0.025,
     vertex.color = V(net_book5)$color, vertex.frame.color= NA,
     vertex.label= V(net_book5)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.5,
     layout = layout.kamada.kawai, main = "GoT Book 4 (Closeness)") 

# barplot closeness centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_closeness_net_b5, decreasing = FALSE), las = 2, col = "green", 
        main = "Closeness Centralities in GoT Book 5",
        names.arg = names(sort(top_closeness_net_b5, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)

# Betweenness (centrality based on a broker position connecting others). 
# Number of geodesics that pass through the node or the edge
betweenness_net_b5 <- betweenness(net_book5, directed='F')
top_betweenness_net_b5 <- sort(betweenness_net_b5, decreasing = TRUE)[1:10]
top_betweenness_net_b5

top_betweenness_indices_b5 <- match(names(top_betweenness_net_b5), names(V(net_book5)))

V(net_book5)$color <- "green"
V(net_book5)$color[top_betweenness_indices_b5] <- "red"
V(net_book5)$size<-1.2
V(net_book5)[top_betweenness_indices_b5]$size<-4

V(net_book5)$label <- NA
V(net_book5)$label[top_betweenness_indices_b5] <- V(net_book5)$name[top_betweenness_indices_b5]

# plot Betweenness
par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book5, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book5)$weight*0.025,
     vertex.color = V(net_book5)$color, vertex.frame.color= NA,
     vertex.label= V(net_book5)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.5,
     layout = layout.kamada.kawai, main = "GoT Book 5 (Betweenness)") 

# barplot Betweenness centralities
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_betweenness_net_b5, decreasing = FALSE), las = 2, col = "green", 
        main = "Betweenness Centralities in GoT Book 5",
        names.arg = names(sort(top_betweenness_net_b5, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Betweenness Centrality",
        horiz = TRUE)

# Eigenvector (centrality proportional to the sum of connection centralities). 
# Values of the first eigenvector of the graph adjacency matrix
eigen_net_b5 <- eigen_centrality(net_book5, directed=F, weights=E(net_book5)$weight)
top_eigen_net_b5 <- sort(eigen_net_b5$vector, decreasing = TRUE)[1:10]
top_eigen_net_b5

top_eigen_indices_b5 <- match(names(top_eigen_net_b5), names(V(net_book5)))

V(net_book5)$color <- "green"
V(net_book5)$color[top_eigen_indices_b5] <- "red"
V(net_book5)$size<-1.2
V(net_book5)[top_eigen_indices_b5]$size<-4

V(net_book5)$label <- NA
V(net_book5)$label[top_eigen_indices_b5] <- V(net_book5)$name[top_eigen_indices_b5]

par(mar = c(0.1, 0.1, 2, 0.1))
plot(net_book5, edge.arrow.size=.0001, edge.curved=0, edge.width = E(net_book5)$weight*0.025,
     vertex.color = V(net_book5)$color, vertex.frame.color= NA,
     vertex.label= V(net_book5)$label, vertex.label.cex = 0.5, vertex.label.color = "black",
     vertex.label.dist = 0.2,
     layout = layout.kamada.kawai, main = "GoT Book 4 (Eigenvector)") 

# barplot eigenvector
par(mar = c(5.1, 8.1, 5.1, 5.1))
barplot(sort(top_eigen_net_b5, decreasing = FALSE), las = 2, col = "green", 
        main = "Eigenvector Centralities in GoT Book 5",
        names.arg = names(sort(top_eigen_net_b5, decreasing = FALSE)), 
        cex.names = 0.8, xlab = "Eigenvector Centrality",
        horiz = TRUE)


# plot all 4 centrality measures:
par(mfrow = c(2, 2), mar = c(4.1, 7.1, 2.1, 4.1))

# barplot degree centralities
barplot(sort(top_degree_net_b5, decreasing = FALSE), las = 2, col = "green", 
        main = "Degree GoT Book 5",
        names.arg = names(sort(top_degree_net_b5, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot closeness centralities
barplot(sort(top_closeness_net_b5, decreasing = FALSE), las = 2, col = "green", 
        main = "Closeness GoT Book 5",
        names.arg = names(sort(top_closeness_net_b5, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot Betweenness centralities
barplot(sort(top_betweenness_net_b5, decreasing = FALSE), las = 2, col = "green", 
        main = "Betweenness GoT Book 5",
        names.arg = names(sort(top_betweenness_net_b5, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
# barplot eigenvector
barplot(sort(top_eigen_net_b5, decreasing = FALSE), las = 2, col = "green", 
        main = "Eigenvector GoT Book 5",
        names.arg = names(sort(top_eigen_net_b5, decreasing = FALSE)), 
        cex.names = 0.8, xlab = NA,
        horiz = TRUE)
par(mfrow = c(1, 1), mar = c(5.1, 8.1, 5.1, 5.1)) # reset plot

# find the most central characters in book 5
centrality_measures_b5 <- list(top_closeness_indices_b5, 
                               top_betweenness_indices_b5, 
                               top_eigen_indices_b5, 
                               top_degree_indices_b5)
common_indices_b5 <- Reduce(intersect, centrality_measures_b5)

# most central characters book 5: 
cc_book5 <- print(V(net_book5)[common_indices_b5])
# Tyrion-Lannister   Daenerys-Targaryen


####################################

# Most central characters by each book:
cc_book1 # Robert-Baratheon Tyrion-Lannister Eddard-Stark 
cc_book2 # Catelyn-Stark     Joffrey-Baratheon Robb-Stark
cc_book3 # Joffrey-Baratheon Tyrion-Lannister  Sansa-Stark       Jaime-Lannister  
cc_book4 # Tyrion-Lannister Cersei-Lannister Jaime-Lannister 
cc_book5 # Tyrion-Lannister   Daenerys-Targaryen

cc_full # Jaime-Lannister  Tyrion-Lannister Cersei-Lannister Jon-Snow 

# lets look at the evolution of character centrality throughout the series

# Lets select prominent characters from the series:
par(mfrow = c(1, 1), mar = c(5.1, 8.1, 5.1, 5.1)) # reset plot

# Robert-Baratheon
robert_centralities <- data.frame(
  Book = 1:5,
  Degree = c(degree(net_book1)["Robert-Baratheon"], 
             degree(net_book2)["Robert-Baratheon"], 
             degree(net_book3)["Robert-Baratheon"], 
             degree(net_book4)["Robert-Baratheon"], 
             degree(net_book5)["Robert-Baratheon"]),
  Closeness = c(closeness(net_book1)["Robert-Baratheon"], 
                closeness(net_book2)["Robert-Baratheon"], 
                closeness(net_book3)["Robert-Baratheon"], 
                closeness(net_book4)["Robert-Baratheon"], 
                closeness(net_book5)["Robert-Baratheon"]),
  Betweenness = c(betweenness(net_book1)["Robert-Baratheon"], 
                  betweenness(net_book2)["Robert-Baratheon"], 
                  betweenness(net_book3)["Robert-Baratheon"], 
                  betweenness(net_book4)["Robert-Baratheon"], 
                  betweenness(net_book5)["Robert-Baratheon"])
  )
plot(robert_centralities$Book, robert_centralities$Degree, type = "b", 
     col = "red", xlab = "Book", ylab = "Centrality",
     main = "Robert Baratheon's Degree Centrality Evolution")
plot(robert_centralities$Book, robert_centralities$Closeness, type = "b", 
     col = "blue", xlab = "Book", ylab = "Centrality",
     main = "Robert Baratheon's Closeness Centrality Evolution")
plot(robert_centralities$Book, robert_centralities$Betweenness, type = "b", 
     col = "green", xlab = "Book", ylab = "Centrality",
     main = "Robert Baratheon's Betweenness Centrality Evolution")

# Tyrion-Lannister
tyrion_centralities <- data.frame(
  Book = 1:5,
  Degree = c(degree(net_book1)["Tyrion-Lannister"], 
             degree(net_book2)["Tyrion-Lannister"], 
             degree(net_book3)["Tyrion-Lannister"], 
             degree(net_book4)["Tyrion-Lannister"], 
             degree(net_book5)["Tyrion-Lannister"]),
  Closeness = c(closeness(net_book1)["Tyrion-Lannister"], 
                closeness(net_book2)["Tyrion-Lannister"], 
                closeness(net_book3)["Tyrion-Lannister"], 
                closeness(net_book4)["Tyrion-Lannister"], 
                closeness(net_book5)["Tyrion-Lannister"]),
  Betweenness = c(betweenness(net_book1)["Tyrion-Lannister"], 
                  betweenness(net_book2)["Tyrion-Lannister"], 
                  betweenness(net_book3)["Tyrion-Lannister"], 
                  betweenness(net_book4)["Tyrion-Lannister"], 
                  betweenness(net_book5)["Tyrion-Lannister"])
)
plot(tyrion_centralities$Book, tyrion_centralities$Degree, type = "b", 
     col = "red", xlab = "Book", ylab = "Centrality",
     main = "Tyrion Lannister's Degree Centrality Evolution")
plot(tyrion_centralities$Book, tyrion_centralities$Closeness, type = "b", 
     col = "blue", xlab = "Book", ylab = "Centrality",
     main = "Tyrion Lannister's Closeness Centrality Evolution")
plot(tyrion_centralities$Book, tyrion_centralities$Betweenness, type = "b", 
     col = "green", xlab = "Book", ylab = "Centrality",
     main = "Tyrion Lannister's Betweenness Centrality Evolution")

# Eddard-Stark
eddard_centralities <- data.frame(
  Book = 1:5,
  Degree = c(degree(net_book1)["Eddard-Stark"], 
             degree(net_book2)["Eddard-Stark"], 
             degree(net_book3)["Eddard-Stark"], 
             degree(net_book4)["Eddard-Stark"], 
             degree(net_book5)["Eddard-Stark"]),
  Closeness = c(closeness(net_book1)["Eddard-Stark"], 
                closeness(net_book2)["Eddard-Stark"], 
                closeness(net_book3)["Eddard-Stark"], 
                closeness(net_book4)["Eddard-Stark"], 
                closeness(net_book5)["Eddard-Stark"]),
  Betweenness = c(betweenness(net_book1)["Eddard-Stark"], 
                  betweenness(net_book2)["Eddard-Stark"], 
                  betweenness(net_book3)["Eddard-Stark"], 
                  betweenness(net_book4)["Eddard-Stark"], 
                  betweenness(net_book5)["Eddard-Stark"])
)
plot(eddard_centralities$Book, eddard_centralities$Degree, type = "b", 
     col = "red", xlab = "Book", ylab = "Centrality",
     main = "Eddard Stark's Degree Centrality Evolution")
plot(eddard_centralities$Book, eddard_centralities$Closeness, type = "b", 
     col = "blue", xlab = "Book", ylab = "Centrality",
     main = "Eddard Stark's Closeness Centrality Evolution")
plot(eddard_centralities$Book, eddard_centralities$Betweenness, type = "b", 
     col = "green", xlab = "Book", ylab = "Centrality",
     main = "Eddard Stark's Betweenness Centrality Evolution")

# Catelyn-Stark
catelyn_centralities <- data.frame(
  Book = 1:5,
  Degree = c(degree(net_book1)["Catelyn-Stark"], 
             degree(net_book2)["Catelyn-Stark"], 
             degree(net_book3)["Catelyn-Stark"], 
             degree(net_book4)["Catelyn-Stark"], 
             degree(net_book5)["Catelyn-Stark"]),
  Closeness = c(closeness(net_book1)["Catelyn-Stark"], 
                closeness(net_book2)["Catelyn-Stark"], 
                closeness(net_book3)["Catelyn-Stark"], 
                closeness(net_book4)["Catelyn-Stark"], 
                closeness(net_book5)["Catelyn-Stark"]),
  Betweenness = c(betweenness(net_book1)["Catelyn-Stark"], 
                  betweenness(net_book2)["Catelyn-Stark"], 
                  betweenness(net_book3)["Catelyn-Stark"], 
                  betweenness(net_book4)["Catelyn-Stark"], 
                  betweenness(net_book5)["Catelyn-Stark"])
)
plot(catelyn_centralities$Book, catelyn_centralities$Degree, type = "b", 
     col = "red", xlab = "Book", ylab = "Centrality",
     main = "Catelyn Stark's Degree Centrality Evolution")
plot(catelyn_centralities$Book, catelyn_centralities$Closeness, type = "b", 
     col = "blue", xlab = "Book", ylab = "Centrality",
     main = "Catelyn Stark's Closeness Centrality Evolution")
plot(catelyn_centralities$Book, catelyn_centralities$Betweenness, type = "b", 
     col = "green", xlab = "Book", ylab = "Centrality",
     main = "Catelyn Stark's Betweenness Centrality Evolution")

# Joffrey-Baratheon
joffrey_centralities <- data.frame(
  Book = 1:5,
  Degree = c(degree(net_book1)["Joffrey-Baratheon"], 
             degree(net_book2)["Joffrey-Baratheon"], 
             degree(net_book3)["Joffrey-Baratheon"], 
             degree(net_book4)["Joffrey-Baratheon"], 
             degree(net_book5)["Joffrey-Baratheon"]),
  Closeness = c(closeness(net_book1)["Joffrey-Baratheon"], 
                closeness(net_book2)["Joffrey-Baratheon"], 
                closeness(net_book3)["Joffrey-Baratheon"], 
                closeness(net_book4)["Joffrey-Baratheon"], 
                closeness(net_book5)["Joffrey-Baratheon"]),
  Betweenness = c(betweenness(net_book1)["Joffrey-Baratheon"], 
                  betweenness(net_book2)["Joffrey-Baratheon"], 
                  betweenness(net_book3)["Joffrey-Baratheon"], 
                  betweenness(net_book4)["Joffrey-Baratheon"], 
                  betweenness(net_book5)["Joffrey-Baratheon"])
)
plot(joffrey_centralities$Book, joffrey_centralities$Degree, type = "b", 
     col = "red", xlab = "Book", ylab = "Centrality",
     main = "Joffrey Baratheon's Degree Centrality Evolution")
plot(joffrey_centralities$Book, joffrey_centralities$Closeness, type = "b", 
     col = "blue", xlab = "Book", ylab = "Centrality",
     main = "Joffrey Baratheon's Closeness Centrality Evolution")
plot(joffrey_centralities$Book, joffrey_centralities$Betweenness, type = "b", 
     col = "green", xlab = "Book", ylab = "Centrality",
     main = "Joffrey Baratheon's Betweenness Centrality Evolution")

# Jaime-Lannister 
jaime_centralities <- data.frame(
  Book = 1:5,
  Degree = c(degree(net_book1)["Jaime-Lannister"], 
             degree(net_book2)["Jaime-Lannister"], 
             degree(net_book3)["Jaime-Lannister"], 
             degree(net_book4)["Jaime-Lannister"], 
             degree(net_book5)["Jaime-Lannister"]),
  Closeness = c(closeness(net_book1)["Jaime-Lannister"], 
                closeness(net_book2)["Jaime-Lannister"], 
                closeness(net_book3)["Jaime-Lannister"], 
                closeness(net_book4)["Jaime-Lannister"], 
                closeness(net_book5)["Jaime-Lannister"]),
  Betweenness = c(betweenness(net_book1)["Jaime-Lannister"], 
                  betweenness(net_book2)["Jaime-Lannister"], 
                  betweenness(net_book3)["Jaime-Lannister"], 
                  betweenness(net_book4)["Jaime-Lannister"], 
                  betweenness(net_book5)["Jaime-Lannister"])
)
plot(jaime_centralities$Book, jaime_centralities$Degree, type = "b", 
     col = "red", xlab = "Book", ylab = "Centrality",
     main = "Jaime Lannister's Degree Centrality Evolution")
plot(jaime_centralities$Book, jaime_centralities$Closeness, type = "b", 
     col = "blue", xlab = "Book", ylab = "Centrality",
     main = "Jaime Lannister's Closeness Centrality Evolution")
plot(jaime_centralities$Book, jaime_centralities$Betweenness, type = "b", 
     col = "green", xlab = "Book", ylab = "Centrality",
     main = "Jaime Lannister's Betweenness Centrality Evolution")

# Cersei-Lannister
cersei_centralities <- data.frame(
  Book = 1:5,
  Degree = c(degree(net_book1)["Cersei-Lannister"], 
             degree(net_book2)["Cersei-Lannister"], 
             degree(net_book3)["Cersei-Lannister"], 
             degree(net_book4)["Cersei-Lannister"], 
             degree(net_book5)["Cersei-Lannister"]),
  Closeness = c(closeness(net_book1)["Cersei-Lannister"], 
                closeness(net_book2)["Cersei-Lannister"], 
                closeness(net_book3)["Cersei-Lannister"], 
                closeness(net_book4)["Cersei-Lannister"], 
                closeness(net_book5)["Cersei-Lannister"]),
  Betweenness = c(betweenness(net_book1)["Cersei-Lannister"], 
                  betweenness(net_book2)["Cersei-Lannister"], 
                  betweenness(net_book3)["Cersei-Lannister"], 
                  betweenness(net_book4)["Cersei-Lannister"], 
                  betweenness(net_book5)["Cersei-Lannister"])
)
plot(cersei_centralities$Book, cersei_centralities$Degree, type = "b", 
     col = "red", xlab = "Book", ylab = "Centrality",
     main = "Cersei Lannister's Degree Centrality Evolution")
plot(cersei_centralities$Book, cersei_centralities$Closeness, type = "b", 
     col = "blue", xlab = "Book", ylab = "Centrality",
     main = "Cersei Lannister's Closeness Centrality Evolution")
plot(cersei_centralities$Book, cersei_centralities$Betweenness, type = "b", 
     col = "green", xlab = "Book", ylab = "Centrality",
     main = "Cersei Lannister's Betweenness Centrality Evolution")

# Daenerys-Targaryen
daenerys_centralities <- data.frame(
  Book = 1:5,
  Degree = c(degree(net_book1)["Daenerys-Targaryen"], 
             degree(net_book2)["Daenerys-Targaryen"], 
             degree(net_book3)["Daenerys-Targaryen"], 
             degree(net_book4)["Daenerys-Targaryen"], 
             degree(net_book5)["Daenerys-Targaryen"]),
  Closeness = c(closeness(net_book1)["Daenerys-Targaryen"], 
                closeness(net_book2)["Daenerys-Targaryen"], 
                closeness(net_book3)["Daenerys-Targaryen"], 
                closeness(net_book4)["Daenerys-Targaryen"], 
                closeness(net_book5)["Daenerys-Targaryen"]),
  Betweenness = c(betweenness(net_book1)["Daenerys-Targaryen"], 
                  betweenness(net_book2)["Daenerys-Targaryen"], 
                  betweenness(net_book3)["Daenerys-Targaryen"], 
                  betweenness(net_book4)["Daenerys-Targaryen"], 
                  betweenness(net_book5)["Daenerys-Targaryen"])
)
plot(daenerys_centralities$Book, daenerys_centralities$Degree, type = "b", 
     col = "red", xlab = "Book", ylab = "Centrality",
     main = "Daenerys Targaryen's Degree Centrality Evolution")
plot(daenerys_centralities$Book, daenerys_centralities$Closeness, type = "b", 
     col = "blue", xlab = "Book", ylab = "Centrality",
     main = "Daenerys Targaryen's Closeness Centrality Evolution")
plot(daenerys_centralities$Book, daenerys_centralities$Betweenness, type = "b", 
     col = "green", xlab = "Book", ylab = "Centrality",
     main = "Daenerys Targaryen's Betweenness Centrality Evolution")

# Jon-Snow 
jon_centralities <- data.frame(
  Book = 1:5,
  Degree = c(degree(net_book1)["Jon-Snow"], 
             degree(net_book2)["Jon-Snow"], 
             degree(net_book3)["Jon-Snow"], 
             degree(net_book4)["Jon-Snow"], 
             degree(net_book5)["Jon-Snow"]),
  Closeness = c(closeness(net_book1)["Jon-Snow"], 
                closeness(net_book2)["Jon-Snow"], 
                closeness(net_book3)["Jon-Snow"], 
                closeness(net_book4)["Jon-Snow"], 
                closeness(net_book5)["Jon-Snow"]),
  Betweenness = c(betweenness(net_book1)["Jon-Snow"], 
                  betweenness(net_book2)["Jon-Snow"], 
                  betweenness(net_book3)["Jon-Snow"], 
                  betweenness(net_book4)["Jon-Snow"], 
                  betweenness(net_book5)["Jon-Snow"])
)
plot(jon_centralities$Book, jon_centralities$Degree, type = "b", 
     col = "red", xlab = "Book", ylab = "Centrality",
     main = "Jon Snow's Degree Centrality Evolution")
plot(jon_centralities$Book, jon_centralities$Closeness, type = "b", 
     col = "blue", xlab = "Book", ylab = "Centrality",
     main = "Jon Snow's Closeness Centrality Evolution")
plot(jon_centralities$Book, jon_centralities$Betweenness, type = "b", 
     col = "green", xlab = "Book", ylab = "Centrality",
     main = "Jon Snow's Betweenness Centrality Evolution")


# plot all characters DEGREE
par(mar=c(5, 4, 4, 8) + 0.1)
plot(robert_centralities$Book, robert_centralities$Degree, type = "l", 
     col = "red", xlab = "Book", ylab = "Centrality",
     ylim = c(0, 80), xlim = c(1, 5),
     main = "GoT Character Degree Centrality Evolution")
lines(tyrion_centralities$Book, tyrion_centralities$Degree, col = "blue")
lines(eddard_centralities$Book, eddard_centralities$Degree, col = "green")
lines(catelyn_centralities$Book, catelyn_centralities$Degree, col = "orange")
lines(joffrey_centralities$Book, joffrey_centralities$Degree, col = "purple")
lines(jaime_centralities$Book, jaime_centralities$Degree, col = "brown")
lines(cersei_centralities$Book, cersei_centralities$Degree, col = "pink")
lines(daenerys_centralities$Book, daenerys_centralities$Degree, col = "cyan", type = "o")
lines(jon_centralities$Book, jon_centralities$Degree, col = "gray")
legend(x="topright", inset=c(-0.465,0),
       legend = c("Robert Baratheon", "Tyrion Lannister", "Eddard Stark", 
                  "Catelyn Stark", "Joffrey Baratheon", "Jaime Lannister", 
                  "Cersei Lannister", "Daenerys Targaryen", "Jon Snow"), 
       col = c("red", "blue", "green", "orange", "purple", "brown", "pink", "cyan", "gray"), 
       lty = 1, cex = 0.7, xpd=TRUE)

# plot all characters CLOSENESS
par(mar=c(5, 4, 4, 8) + 0.1)
plot(robert_centralities$Book, robert_centralities$Closeness, type = "l", 
     col = "red", xlab = "Book", ylab = "Centrality",
     ylim = c(0.0002, 0.00057), xlim = c(1, 5),
     main = "GoT Character Closeness Centrality Evolution")
lines(tyrion_centralities$Book, tyrion_centralities$Closeness, col = "blue")
lines(eddard_centralities$Book, eddard_centralities$Closeness, col = "green")
lines(catelyn_centralities$Book, catelyn_centralities$Closeness, col = "orange")
lines(joffrey_centralities$Book, joffrey_centralities$Closeness, col = "purple")
lines(jaime_centralities$Book, jaime_centralities$Closeness, col = "brown")
lines(cersei_centralities$Book, cersei_centralities$Closeness, col = "pink")
lines(daenerys_centralities$Book, daenerys_centralities$Closeness, col = "cyan", type = "o")
lines(jon_centralities$Book, jon_centralities$Closeness, col = "gray")
legend(x="topright", inset=c(-0.465,0),
       legend = c("Robert Baratheon", "Tyrion Lannister", "Eddard Stark", 
                  "Catelyn Stark", "Joffrey Baratheon", "Jaime Lannister", 
                  "Cersei Lannister", "Daenerys Targaryen", "Jon Snow"), 
       col = c("red", "blue", "green", "orange", "purple", "brown", "pink", "cyan", "gray"), 
       lty = 1, cex = 0.7, xpd=TRUE)

# plot all characters BETWEENNESS
par(mar=c(5, 4, 4, 8) + 0.1)
plot(robert_centralities$Book, robert_centralities$Betweenness, type = "l", 
     col = "red", xlab = "Book", ylab = "Centrality",
     ylim = c(0, 13000), xlim = c(1, 5),
     main = "GoT Character Betweenness Centrality Evolution")
lines(tyrion_centralities$Book, tyrion_centralities$Betweenness, col = "blue")
lines(eddard_centralities$Book, eddard_centralities$Betweenness, col = "green")
lines(catelyn_centralities$Book, catelyn_centralities$Betweenness, col = "orange")
lines(joffrey_centralities$Book, joffrey_centralities$Betweenness, col = "purple")
lines(jaime_centralities$Book, jaime_centralities$Betweenness, col = "brown")
lines(cersei_centralities$Book, cersei_centralities$Betweenness, col = "pink")
lines(daenerys_centralities$Book, daenerys_centralities$Betweenness, col = "cyan", type = "o")
lines(jon_centralities$Book, jon_centralities$Betweenness, col = "gray")
legend(x="topright", inset=c(-0.465,0),
       legend = c("Robert Baratheon", "Tyrion Lannister", "Eddard Stark", 
                  "Catelyn Stark", "Joffrey Baratheon", "Jaime Lannister", 
                  "Cersei Lannister", "Daenerys Targaryen", "Jon Snow"), 
       col = c("red", "blue", "green", "orange", "purple", "brown", "pink", "cyan", "gray"), 
       lty = 1, cex = 0.7, xpd=TRUE)


