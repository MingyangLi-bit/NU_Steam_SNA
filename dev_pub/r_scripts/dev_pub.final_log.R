# SNA for developers and publishers co-authorship analysis for STEAM apps

# Clear env
rm(list=ls())
setwd('/Users/mingyangli/Desktop/NU Courses/2023Fall/CS496_SNA/Final Project/r_scripts/dev_pub_final')

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(igraph)
library(ggraph)
library(network)

# Read the data
dev_attributes <- read.csv("dev_attributes.csv")
pub_attributes <- read.csv("pub_attributes.csv")
dev_pub_edge_list <- read.csv("dev_pub_edge_list.csv")

# take log transformation
attr_list <- c("Number.of.Games", "Number.of.DLCs", "Total.Games.and.DLCs", "percent_free", "Total_Reviews", "Total_Positive_Reviews", "Total_Negative_Reviews", "Average_Review_Score", "Percentage_of_Positive_Reviews", "Steam_Workshop_Percentage", "Steam_Cloud_Percentage", "Steam_Achievements_Percentage", "In_App_Purchases_Percentage")
for (attr in attr_list) {
  dev_attributes[[paste0("log_", attr)]] <- log(dev_attributes[[attr]] + 1)
  pub_attributes[[paste0("log_", attr)]] <- log(pub_attributes[[attr]] + 1)
}

# Preprocess edge list for bipartite network
edge_list <- data.frame(from = dev_pub_edge_list$developer_id, to = dev_pub_edge_list$publisher_id, weight = dev_pub_edge_list$weight)

# Basic summary
summary(dev_attributes)
summary(pub_attributes)
summary(dev_pub_edge_list)

# # Histogram of Number of Games by Developers
# ggplot(dev_attributes, aes(x = `log_Number.of.Games`)) +
#   geom_histogram(bins = 100, fill = "blue", color = "black") +
#   ggtitle("Histogram of Number of Games by Developers") +
#   xlab("Number of Games") + ylab("Count")
# 
# # Histogram of Number of DLCs by Developers
# ggplot(dev_attributes, aes(x = `log_Number.of.DLCs`)) +
#   geom_histogram(bins = 100, fill = "red", color = "black") +
#   ggtitle("Histogram of Number of DLCs by Developers") +
#   xlab("Number of DLCs") + ylab("Count")
# 
# # Histogram of the Percentage of Free Games by Developers
# ggplot(dev_attributes, aes(x = log_percent_free)) +
#   geom_histogram(bins = 100, fill = "green", color = "black") +
#   ggtitle("Histogram of Free Percentage by Developers") +
#   xlab("Percent Free") + ylab("Count")
# 
# # Histogram of Total Reviews by Developers
# ggplot(dev_attributes, aes(x = `log_Total_Reviews`)) +
#   geom_histogram(bins = 100, fill = "cyan", color = "black") +
#   ggtitle("Histogram of Total Reviews by Developers") +
#   xlab("Total Reviews") + ylab("Count")
# 
# # Histogram of Total Positive Reviews by Developers
# ggplot(dev_attributes, aes(x = `log_Total_Positive_Reviews`)) +
#   geom_histogram(bins = 100, fill = "purple", color = "black") +
#   ggtitle("Histogram of Total Positive Reviews by Developers") +
#   xlab("Total Positive Reviews") + ylab("Count")
# 
# # Histogram of Total Negative Reviews by Developers
# ggplot(dev_attributes, aes(x = `log_Total_Negative_Reviews`)) +
#   geom_histogram(bins = 100, fill = "orange", color = "black") +
#   ggtitle("Histogram of Total Negative Reviews by Developers") +
#   xlab("Total Negative Reviews") + ylab("Count")
# 
# # Histogram of Average Review Score by Developers
# ggplot(dev_attributes, aes(x = `log_Average_Review_Score`)) +
#   geom_histogram(bins = 100, fill = "yellow", color = "black") +
#   ggtitle("Histogram of Average Review Score by Developers") +
#   xlab("Average Review Score") + ylab("Count")
# 
# # Histogram of Percentage of Positive Reviews by Developers
# ggplot(dev_attributes, aes(x = `log_Percentage_of_Positive_Reviews`)) +
#   geom_histogram(bins = 100, fill = "pink", color = "black") +
#   ggtitle("Histogram of Percentage of Positive Reviews by Developers") +
#   xlab("Percentage of Positive Reviews") + ylab("Count")
# 
# # Histogram of Steam Workshop Percentage by Developers
# ggplot(dev_attributes, aes(x = `log_Steam_Workshop_Percentage`)) +
#   geom_histogram(bins = 100, fill = "lightblue", color = "black") +
#   ggtitle("Histogram of Steam Workshop Percentage by Developers") +
#   xlab("Steam Workshop Percentage") + ylab("Count")
# 
# # Histogram of Steam Cloud Percentage by Developers
# ggplot(dev_attributes, aes(x = `log_Steam_Cloud_Percentage`)) +
#   geom_histogram(bins = 100, fill = "lightgreen", color = "black") +
#   ggtitle("Histogram of Steam Cloud Percentage by Developers") +
#   xlab("Steam Cloud Percentage") + ylab("Count")
# 
# # Histogram of Steam Achievements Percentage by Developers
# ggplot(dev_attributes, aes(x = `log_Steam_Achievements_Percentage`)) +
#   geom_histogram(bins = 100, fill = "red", color = "black") +
#   ggtitle("Histogram of Steam Achievements Percentage by Developers") +
#   xlab("Steam Achievements Percentage") + ylab("Count")
# 
# # Histogram of In-App Purchases Percentage by Developers
# ggplot(dev_attributes, aes(x = `log_In_App_Purchases_Percentage`)) +
#   geom_histogram(bins = 100, fill = "blue", color = "black") +
#   ggtitle("Histogram of In-App Purchases Percentage by Developers") +
#   xlab("In-App Purchases Percentage") + ylab("Count")
# 
# # Histogram of Number of Games by Publishers
# ggplot(pub_attributes, aes(x = `log_Number.of.Games`)) +
#   geom_histogram(bins = 100, fill = "blue", color = "black") +
#   ggtitle("Histogram of Number of Games by Publishers") +
#   xlab("Number of Games") + ylab("Count")
# 
# # Histogram of Number of DLCs by Publishers
# ggplot(pub_attributes, aes(x = `log_Number.of.DLCs`)) +
#   geom_histogram(bins = 100, fill = "red", color = "black") +
#   ggtitle("Histogram of Number of DLCs by Publishers") +
#   xlab("Number of DLCs") + ylab("Count")
# 
# # Histogram of the Percentage of Free Games by Publishers
# ggplot(pub_attributes, aes(x = log_percent_free)) +
#   geom_histogram(bins = 100, fill = "green", color = "black") +
#   ggtitle("Histogram of Free Percentage by Publishers") +
#   xlab("Percent Free") + ylab("Count")
# 
# # Histogram of Total Reviews by Publishers
# ggplot(pub_attributes, aes(x = `log_Total_Reviews`)) +
#   geom_histogram(bins = 100, fill = "cyan", color = "black") +
#   ggtitle("Histogram of Total Reviews by Publishers") +
#   xlab("Total Reviews") + ylab("Count")
# 
# # Histogram of Total Positive Reviews by Publishers
# ggplot(pub_attributes, aes(x = `log_Total_Positive_Reviews`)) +
#   geom_histogram(bins = 100, fill = "purple", color = "black") +
#   ggtitle("Histogram of Total Positive Reviews by Publishers") +
#   xlab("Total Positive Reviews") + ylab("Count")
# 
# # Histogram of Total Negative Reviews by Publishers
# ggplot(pub_attributes, aes(x = `log_Total_Negative_Reviews`)) +
#   geom_histogram(bins = 100, fill = "orange", color = "black") +
#   ggtitle("Histogram of Total Negative Reviews by Publishers") +
#   xlab("Total Negative Reviews") + ylab("Count")
# 
# # Histogram of Average Review Score by Publishers
# ggplot(pub_attributes, aes(x = `log_Average_Review_Score`)) +
#   geom_histogram(bins = 100, fill = "yellow", color = "black") +
#   ggtitle("Histogram of Average Review Score by Publishers") +
#   xlab("Average Review Score") + ylab("Count")
# 
# # Histogram of Percentage of Positive Reviews by Publishers
# ggplot(pub_attributes, aes(x = `log_Percentage_of_Positive_Reviews`)) +
#   geom_histogram(bins = 100, fill = "pink", color = "black") +
#   ggtitle("Histogram of Percentage of Positive Reviews by Publishers") +
#   xlab("Percentage of Positive Reviews") + ylab("Count")
# 
# # Histogram of Steam Workshop Percentage by Publishers
# ggplot(pub_attributes, aes(x = `log_Steam_Workshop_Percentage`)) +
#   geom_histogram(bins = 100, fill = "#B0C4DE", color = "black") +  # Light blue
#   ggtitle("Histogram of Steam Workshop Percentage by Publishers") +
#   xlab("Steam Workshop Percentage") + ylab("Count")
# 
# # Histogram of Steam Cloud Percentage by Publishers
# ggplot(pub_attributes, aes(x = `log_Steam_Cloud_Percentage`)) +
#   geom_histogram(bins = 100, fill = "#90EE90", color = "black") +  # Light green
#   ggtitle("Histogram of Steam Cloud Percentage by Publishers") +
#   xlab("Steam Cloud Percentage") + ylab("Count")
# 
# # Histogram of Steam Achievements Percentage by Publishers
# ggplot(pub_attributes, aes(x = `log_Steam_Achievements_Percentage`)) +
#   geom_histogram(bins = 100, fill = "#FFC0CB", color = "black") +  # Light red (pink)
#   ggtitle("Histogram of Steam Achievements Percentage by Publishers") +
#   xlab("Steam Achievements Percentage") + ylab("Count")
# 
# # Histogram of In-App Purchases Percentage by Publishers
# ggplot(pub_attributes, aes(x = `log_In_App_Purchases_Percentage`)) +
#   geom_histogram(bins = 100, fill = "#C7A9D9", color = "black") +  # Light purple
#   ggtitle("Histogram of In-App Purchases Percentage by Publishers") +
#   xlab("In-App Purchases Percentage") + ylab("Count")

# Create a bipartite graph
network <- graph_from_data_frame(d = edge_list, directed = FALSE)
# Ensure all nodes are assigned a type
dev_ids <- unique(dev_pub_edge_list$developer_id)
pub_ids <- unique(dev_pub_edge_list$publisher_id)
V(network)$type <- ifelse(V(network)$name %in% dev_ids, "Developer", "Publisher")

# Set node attributes for visualization
V(network)$color <- ifelse(V(network)$type == "Developer", "red", "green")
V(network)$shape <- ifelse(V(network)$type == "Developer", "square", "circle")

# Remove self-loops and multiple edges, if any
if (anyNA(E(network)$from) || anyNA(E(network)$to)) {
  print("There are NA values in the edge list")
}
network <- simplify(network, remove.multiple = TRUE, remove.loops = TRUE)
if (anyNA(V(network)$type)) {
  print("NA values found in vertex types after operation")
}

# Nodes and Edges
vcount(network) # the number of nodes
ecount(network) # the number of edges

# Local Centrality Measures
degree_centrality <- degree(network)
closeness_centrality <- closeness(network)
betweenness_centrality <- betweenness(network)

# Print local centrality measures
print(degree_centrality)
print(closeness_centrality)
print(betweenness_centrality)

# Global Centrality Measures
network_density <- graph.density(network)
network_diameter <- diameter(network)

# Print global centrality measures
print(network_density)
print(network_diameter)

# Visualization

# Apply log transformation to weights
E(network)$log_weight <- log1p(E(network)$weight)

# Calculating the components of the network
network_components <- components(network)

# Extracting the largest component
largest_component <- induced.subgraph(network, which(network_components$membership == which.max(network_components$csize)))

# Visualization using ggraph with adjusted log-transformed weights
ggraph(network, layout = "kk") + 
  geom_edge_link(aes(edge_width = log_weight), alpha = 1, edge_alpha = 0.1, lineend = "round") + 
  geom_node_point(aes(color = type, size = degree(network, mode = "all") / 2), alpha = 1) +  # Scale down the node size
  scale_edge_width(range = c(0.5, 1)) +  # Adjust the range for edge width
  scale_color_brewer(palette = "Set1", name = "Node Type", labels = c("Developer", "Publisher")) +
  theme_void() +
  theme(legend.position = "right") +
  labs(edge_width = "Log-transformed Weight")

# Visualization ofthe largest component using ggraph with adjusted log-transformed weights
ggraph(largest_component, layout = "kk") + 
  geom_edge_link(aes(edge_width = log_weight), alpha = 1, edge_alpha = 0.1, lineend = "round") + 
  geom_node_point(aes(color = type, size = degree(largest_component, mode = "all") / 2), alpha = 1) +  # Scale down the node size
  scale_edge_width(range = c(0.5, 1)) +  # Adjust the range for edge width
  scale_color_brewer(palette = "Set1", name = "Node Type", labels = c("Developer", "Publisher")) +
  theme_void() +
  theme(legend.position = "right") +
  labs(edge_width = "Log-transformed Weight")

# Visualization of the largest component using ggraph with adjusted log-transformed weights
ggraph(largest_component, layout = "kk") + 
  geom_edge_link(aes(edge_width = log_weight), alpha = 0.1, lineend = "round") + 
  geom_node_point(aes(color = type, size = degree(largest_component, mode = "all") / 2), alpha = 1) +
  geom_node_text(aes(label = name, color = type), size = 3, vjust = 1.5, hjust = 0.5) + # Add node labels
  scale_edge_width(range = c(0.5, 1)) +  # Adjust the range for edge width
  scale_color_brewer(palette = "Set1", name = "Node Type", labels = c("Developer", "Publisher")) +
  theme_void() +
  theme(legend.position = "right") +
  labs(edge_width = "Log-transformed Weight")

# For developers
dev_names <- dev_pub_edge_list %>%
  select(developer_id, developers) %>%
  distinct()

# For publishers
pub_names <- dev_pub_edge_list %>%
  select(publisher_id, publishers) %>%
  distinct()

# Assuming 'largest_component' already has a 'type' attribute
# Add the actual names to the nodes
V(largest_component)$name <- ifelse(V(largest_component)$type == "Developer",
                                    dev_names[match(V(largest_component)$name, dev_names$developer_id), "developers"],
                                    pub_names[match(V(largest_component)$name, pub_names$publisher_id), "publishers"])

# Now create the ggraph plot using the actual names
ggraph(largest_component, layout = "kk") + 
  geom_edge_link(aes(edge_width = log_weight), alpha = 0.1, lineend = "round") + 
  geom_node_point(aes(color = type, size = degree(largest_component, mode = "all") / 2), alpha = 1) +
  geom_node_text(aes(label = name, color = type), size = 3, vjust = 1.5, hjust = 0.5) + # Add actual names as node labels
  scale_edge_width(range = c(0.5, 1)) +  # Adjust the range for edge width
  scale_color_brewer(palette = "Set1", name = "Node Type", labels = c("Developer", "Publisher")) +
  theme_void() +
  theme(legend.position = "right") +
  labs(edge_width = "Log-transformed Weight")

network <- set_vertex_attr(network, "type", value = ifelse(V(network)$name %in% dev_attributes$developer_id, "Developer", "Publisher"))
V(network)$color <- ifelse(V(network)$type == "Developer", "red", "green")
V(network)$shape <- ifelse(V(network)$type == "Developer", "square", "circle")


# library(ergm)
attr_list <- c("Number.of.Games", "Number.of.DLCs", "Total.Games.and.DLCs", "percent_free", "Total_Reviews", "Total_Positive_Reviews", "Total_Negative_Reviews", "Average_Review_Score", "Percentage_of_Positive_Reviews", "Steam_Workshop_Percentage", "Steam_Cloud_Percentage", "Steam_Achievements_Percentage", "In_App_Purchases_Percentage", "log_Number.of.Games", "log_Number.of.DLCs", "log_Total.Games.and.DLCs", "log_percent_free", "log_Total_Reviews", "log_Total_Positive_Reviews", "log_Total_Negative_Reviews", "log_Average_Review_Score", "log_Percentage_of_Positive_Reviews", "log_Steam_Workshop_Percentage", "log_Steam_Cloud_Percentage", "log_Steam_Achievements_Percentage", "log_In_App_Purchases_Percentage", "free", "cloud", "workshop", "achievement", "purchase")

# Set vertex attributes for developers
dev_ids <- V(network)$name[which(V(network)$type == "Developer")]
for (dev_id in dev_ids) {
  if (dev_id %in% dev_attributes$developer_id) {
    attributes_row <- dev_attributes[dev_attributes$developer_id == dev_id, ]
    for (attr_name in names(dev_attributes)[-1]) {  # Exclude the ID column
      network <- set_vertex_attr(network, attr_name, index = which(V(network)$name == dev_id), value = attributes_row[[attr_name]])
    }
  }
}

# Set vertex attributes for publishers
pub_ids <- V(network)$name[which(V(network)$type == "Publisher")]
for (pub_id in pub_ids) {
  if (pub_id %in% pub_attributes$publisher_id) {
    attributes_row <- pub_attributes[pub_attributes$publisher_id == pub_id, ]
    for (attr_name in names(pub_attributes)[-1]) {  # Exclude the ID column
      network <- set_vertex_attr(network, attr_name, index = which(V(network)$name == pub_id), value = attributes_row[[attr_name]])
    }
  }
}

network <- delete_vertex_attr(network, "developer_id")
network <- delete_vertex_attr(network, "publisher_id")


edge_list <- get.edgelist(network)
vertex_attrs <- vertex_attr(network)
edge_attrs <- edge_attr(network)

bipartite_vector <- ifelse(vertex_attrs$type == "Developer", 1, 0)

num_set1 <- sum(bipartite_vector)

net_network <- network(edge_list, directed = FALSE, bipartite = num_set1)

for (attr_name in names(vertex_attrs)) {
  net_network %v% attr_name <- vertex_attrs[[attr_name]]
}

# Transfer edge attributes
for (attr_name in names(edge_attrs)) {
  net_network %e% attr_name <- edge_attrs[[attr_name]]
}
detach(package:igraph) # Remove the 'igraph' package from your environment. 
library(statnet)
library(ergm)
ergm_model <- ergm(net_network ~ edges +
                   nodecov('Average_Review_Score') +
                   nodecov('log_Total.Games.and.DLCs') +
                   nodecov('log_Total_Reviews') +
                   nodematch('free') +
                   nodematch('cloud') +
                   nodematch('workshop') +
                   nodematch('achievement') +
                   nodematch('purchase'),
                   control = control.ergm(MCMC.effectiveSize = 500
                   ))
summary(ergm_model)


ergm_model <- ergm(net_network ~ edges +
                     # b1degree(c(1)) +
                     # b2degree(c(2)) +
                     # degcor+
                     # degcrossprod+
                     degree1.5,
                     # gwb1degree(decay = 0.5, fixed = TRUE),
                     # nodematch("achievement") +
                     # nodematch("workshop") +
                     # nodematch("free") +
                     # nodecov('Average_Review_Score') +
                     # nodecov('Percentage_of_Positive_Reviews'),
                   control = control.ergm(MCMC.effectiveSize = 500)
                     )


# Summary of the fitted model
summary(ergm_model)
par(mar=c(5.1, 4.1, 4.1, 2.1))
mcmc.diagnostics(ergm_model)

gof_ergm <- gof(ergm_model)
print(gof_ergm)
plot(gof_ergm)

