# Start with a clear environment
rm(list=ls())

# Install packages
library(igraph)
library(network)
library(devtools)
library(ggplot2)
library(ggraph)
library(tidyverse)

# Set working directory
setwd('/Users/mingyangli/Desktop/NU Courses/2023Fall/CS496_SNA/Final Project/r_scripts/genres_unweighted_yearly')
list.files()

# Number of time points
num_time_points <- 10

# Lists to store data for each time point
network_graphs <- list()

# Read in sociomatrices
genres.data.t1 <- as.matrix(read.table("genres_edge_2013_newly_added.dat"))
genres.data.t2 <- as.matrix(read.table("genres_edge_2014_newly_added.dat"))
genres.data.t3 <- as.matrix(read.table("genres_edge_2015_newly_added.dat"))
genres.data.t4 <- as.matrix(read.table("genres_edge_2016_newly_added.dat"))
genres.data.t5 <- as.matrix(read.table("genres_edge_2017_newly_added.dat"))
genres.data.t6 <- as.matrix(read.table("genres_edge_2018_newly_added.dat"))
genres.data.t7 <- as.matrix(read.table("genres_edge_2019_newly_added.dat"))
genres.data.t8 <- as.matrix(read.table("genres_edge_2020_newly_added.dat"))
genres.data.t9 <- as.matrix(read.table("genres_edge_2021_newly_added.dat"))
genres.data.t10 <- as.matrix(read.table("genres_edge_2022_newly_added.dat"))

# Convert to graph
graph1 <- graph_from_adjacency_matrix(genres.data.t1, mode = "undirected")
graph2 <- graph_from_adjacency_matrix(genres.data.t2, mode = "undirected")
graph3 <- graph_from_adjacency_matrix(genres.data.t3, mode = "undirected")
graph4 <- graph_from_adjacency_matrix(genres.data.t4, mode = "undirected")
graph5 <- graph_from_adjacency_matrix(genres.data.t5, mode = "undirected")
graph6 <- graph_from_adjacency_matrix(genres.data.t6, mode = "undirected")
graph7 <- graph_from_adjacency_matrix(genres.data.t7, mode = "undirected")
graph8 <- graph_from_adjacency_matrix(genres.data.t8, mode = "undirected")
graph9 <- graph_from_adjacency_matrix(genres.data.t9, mode = "undirected")
graph10 <- graph_from_adjacency_matrix(genres.data.t10, mode = "undirected")

# Assign names to graphs
names <- c('Action', 'Adventure', 'Animation & Modeling', 'Audio Production', 'Casual', 'Design & Illustration', 'Early Access', 'Education', 'Free to Play', 'Game Development', 'Gore', 'Indie', 'Massively Multiplayer', 'Nudity', 'Photo Editing', 'RPG', 'Racing', 'Sexual Content', 'Simulation', 'Software Training', 'Sports', 'Strategy', 'Utilities', 'Video Production', 'Violent', 'Web Publishing')
V(graph1)$name <- names
V(graph2)$name <- names
V(graph3)$name <- names
V(graph4)$name <- names
V(graph5)$name <- names
V(graph6)$name <- names
V(graph7)$name <- names
V(graph8)$name <- names
V(graph9)$name <- names
V(graph10)$name <- names

# Visualize all graphs
graphs <- list(graph1, graph2, graph3, graph4, graph5, graph6, graph7, graph8, graph9, graph10)
for(graph in graphs) {
  g = ggraph(graph, layout = 'kk') + 
    geom_edge_link(aes(width = 0.1), edge_colour = 'grey', alpha = 1) +
    geom_node_point(aes(size = degree(graph, mode = "all") / 2), color = 'black', alpha = 1) +
    # geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
    geom_node_text(aes(label = name), size = 3, vjust = 3, hjust = 0.5, fontface = "bold") + 
    scale_edge_width(range = c(0.5, 1)) +
    theme_void() +
    theme(legend.position = "none")+
    labs(title = 'Network Visualization 2022')
  print(g)
}
library(viridis)
g1 = ggraph(graph5, layout = 'kk') + 
  geom_edge_link(aes(width = 0.1), edge_colour = 'grey', alpha = 1) +
  geom_node_point(aes(size = degree(graph5, mode = "all") / 2), color = 'red', alpha = 1) +
  # geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
  geom_node_text(aes(label = name), size = 3, vjust = 3, hjust = 0.5, fontface = "bold") + 
  scale_edge_width(range = c(0.5, 1)) +
  theme_void() +
  theme(legend.position = "none")+
  labs(title = 'Network Visualization 2018')
print(g1)

g2 = ggraph(graph10, layout = 'kk') + 
  geom_edge_link(aes(width = 0.1), edge_colour = 'grey', alpha = 1) +
  geom_node_point(aes(size = degree(graph5, mode = "all") / 2), color = 'red', alpha = 1) +
  # geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
  geom_node_text(aes(label = name), size = 3, vjust = 3, hjust = 0.5, fontface = "bold") + 
  scale_edge_width(range = c(0.5, 1)) +
  theme_void() +
  theme(legend.position = "none")+
  labs(title = 'Network Visualization 2018')
print(g2)

for(graph in graphs) {
  degree_dist <- degree(graph)
  hist(degree_dist, main="Degree Distribution", xlab="Degree", ylab="Frequency")
}


degree_centrality <- degree(graph5)
closeness_centrality <- closeness(graph5)
eigenvector_centrality <- eigen_centrality(graph5)$vector
constraints <- constraint(graph5)
betweenness_centrality <- betweenness(graph5, directed = FALSE)

centrality_df <- data.frame(
  node = names(degree_centrality),
  degree = degree_centrality,
  eigenvector = eigenvector_centrality,
  constraint = constraints,
  betweenness_centrality
)

library(tidyr)
long_centrality_df <- pivot_longer(centrality_df, cols = -node, names_to = "centrality_type", values_to = "value")
long_centrality_df <- long_centrality_df %>% 
  arrange(desc(value), .by_group = TRUE) %>%
  mutate(node = factor(node, levels = unique(node)))

ggplot(long_centrality_df, aes(x = node, y = value, fill = centrality_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~centrality_type, scales = "free_y") +
  theme_minimal() +
  labs(x = "Node", y = "Centrality", title = "Network Centrality Measures") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Dark2") # Adds color, optional

