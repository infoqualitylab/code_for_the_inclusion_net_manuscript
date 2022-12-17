#
# salt_community_detection.R
#
# This file generates the community detection results for salt controversy inclusion network
# reported in manuscript "Exploring Evidence Selection with the Inclusion Network"
# by Yuanxi Fu, Caitlin Vitosky Clarke, Mark Van Moer, and Jodi Schneider

# author: Yuanxi Fu
#
# Where to Find the data
#
# ExRx: Clarke, Caitlin; Lischwe Mueller, Natalie; Joshi, Manasi Ballal; Fu, Yuanxi; Schneider, Jodi (2022): 
# The Inclusion Network of 27 Review Articles Published between 2013-2018 
# Investigating the Relationship Between Physical Activity and Depressive Symptoms. 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-4614455_V1
#
# Salt: Fu, Yuanxi; Hsiao, Tzu-Kun; Joshi, Manasi Ballal (2022): 
# The Salt Controversy Systematic Review Reports and Primary Study Reports Network Dataset . 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-6128763_V2

rm(list = ls())
source("functions.R")

# set input files: Salt Controversy
raw_edge_list_file_salt <- "data/salt/inclusion_net_edges.csv"
raw_attr_list_file_salt <- "data/salt/report_list.csv"
raw_search_date_file_salt <- "data/salt/systematic_review_inclusion_criteria.csv"

G_salt <- create_salt_graph(raw_edge_list_file_salt, 
                            raw_attr_list_file_salt, 
                            raw_search_date_file_salt)

# select the connected component
comp_list <- components(G_salt)
largest_component_id <- which.max(comp_list$csize)
largest_component_vids <- V(G_salt)[comp_list$membership == largest_component_id]
largest_comp <- igraph::induced_subgraph(G_salt, largest_component_vids)

# community detection
largest_comp_undirected <- as.undirected(largest_comp)
btw <- cluster_edge_betweenness(largest_comp_undirected)
evc <- cluster_leading_eigen(largest_comp_undirected) 
fgc <- cluster_fast_greedy(largest_comp_undirected)
inf <- infomap.community(largest_comp_undirected)
lpc <- label.propagation.community(largest_comp_undirected)
mlc <- multilevel.community(largest_comp_undirected)
sgc <- spinglass.community(largest_comp_undirected)
wtc <- walktrap.community(largest_comp_undirected)
louvain <- cluster_louvain(largest_comp_undirected)

# visualization
# my_layout <- igraph::layout_with_kk(largest_comp_undirected)
# my_layout <- igraph::layout.fruchterman.reingold.grid(largest_comp_undirected)
my_layout <- igraph::layout.fruchterman.reingold(largest_comp_undirected)
fig_width = 400
fig_height = 400
fig_bottom = 0.2
fig_left = 0.2
fig_right = 0.2
fig_top = 0.2


# btw: cluster_edge_betweenness
V(largest_comp_undirected)$membership <- btw$membership

node_color <- case_when(
  V(largest_comp_undirected)$membership == "1" ~ "orange",
  V(largest_comp_undirected)$membership == "2" ~ "pink",
  V(largest_comp_undirected)$membership == "3" ~ "skyblue",
  V(largest_comp_undirected)$membership == "4" ~ "yellow"
)

node_shape <- case_when(
  
  V(largest_comp_undirected)$type == "Systematic Review Report" ~ "square",
  V(largest_comp_undirected)$type == "Primary Study Report" ~ 'circle'
  
)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right))
plot(largest_comp_undirected, vertex.size=14, 
     edge.arrow.width = 0.1, 
     edge.arrow.size = 0.1,
     vertex.color = node_color,
     vertex.shape = node_shape,
     layout = my_layout
)


dev.print(device = jpeg,
          filename = "edge_betweenness.jpg",
          width = fig_width,
          height = fig_height)


dev.off()

# evc: cluster_leading_eigen
V(largest_comp_undirected)$membership <- evc$membership

node_color <- case_when(
  V(largest_comp_undirected)$membership == "1" ~ "orange",
  V(largest_comp_undirected)$membership == "2" ~ "pink",
  V(largest_comp_undirected)$membership == "3" ~ "skyblue",
  V(largest_comp_undirected)$membership == "4" ~ "yellow"
)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right))
plot(largest_comp_undirected, vertex.size=14, 
     edge.arrow.width = 0.1, 
     edge.arrow.size = 0.1,
     vertex.color = node_color,
     vertex.shape = node_shape,
     layout = my_layout
)


dev.print(device = jpeg,
          filename = "cluster_leading_eigen.jpg",
          width = fig_width,
          height = fig_height)


dev.off()


# fgc: cluster_fast_greedy
V(largest_comp_undirected)$membership <- fgc$membership

node_color <- case_when(
  V(largest_comp_undirected)$membership == "1" ~ "orange",
  V(largest_comp_undirected)$membership == "2" ~ "pink",
  V(largest_comp_undirected)$membership == "3" ~ "skyblue",
  V(largest_comp_undirected)$membership == "4" ~ "yellow"
)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right))
plot(largest_comp_undirected, vertex.size=14, 
     edge.arrow.width = 0.1, 
     edge.arrow.size = 0.1,
     vertex.color = node_color,
     vertex.shape = node_shape,
     layout = my_layout
)


dev.print(device = jpeg,
          filename = "cluster_fast_greedy.jpg",
          width = fig_width,
          height = fig_height)


dev.off()

# inf: infomap.community
V(largest_comp_undirected)$membership <- inf$membership

node_color <- case_when(
  V(largest_comp_undirected)$membership == "1" ~ "orange",
  V(largest_comp_undirected)$membership == "2" ~ "pink",
  V(largest_comp_undirected)$membership == "3" ~ "skyblue",
  V(largest_comp_undirected)$membership == "4" ~ "yellow"
)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right))
plot(largest_comp_undirected, vertex.size=14, 
     edge.arrow.width = 0.1, 
     edge.arrow.size = 0.1,
     vertex.color = node_color,
     vertex.shape = node_shape,
     layout = my_layout
)


dev.print(device = jpeg,
          filename = "infomap_community.jpg",
          width = fig_width,
          height = fig_height)


dev.off()

# lpc: label.propagation.community
V(largest_comp_undirected)$membership <- lpc$membership

node_color <- case_when(
  V(largest_comp_undirected)$membership == "1" ~ "orange",
  V(largest_comp_undirected)$membership == "2" ~ "pink",
  V(largest_comp_undirected)$membership == "3" ~ "skyblue",
  V(largest_comp_undirected)$membership == "4" ~ "yellow"
)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right))
plot(largest_comp_undirected, vertex.size=14, 
     edge.arrow.width = 0.1, 
     edge.arrow.size = 0.1,
     vertex.color = node_color,
     vertex.shape = node_shape,
     layout = my_layout
)


dev.print(device = jpeg,
          filename = "label_propagation_community.jpg",
          width = fig_width,
          height = fig_height)


dev.off()


# mlc: multilevel.community
V(largest_comp_undirected)$membership <- mlc$membership

node_color <- case_when(
  V(largest_comp_undirected)$membership == "1" ~ "orange",
  V(largest_comp_undirected)$membership == "2" ~ "pink",
  V(largest_comp_undirected)$membership == "3" ~ "skyblue",
  V(largest_comp_undirected)$membership == "4" ~ "yellow"
)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right))
plot(largest_comp_undirected, vertex.size=14, 
     edge.arrow.width = 0.1, 
     edge.arrow.size = 0.1,
     vertex.color = node_color,
     vertex.shape = node_shape,
     layout = my_layout
)


dev.print(device = jpeg,
          filename = "multilevel_community.jpg",
          width = fig_width,
          height = fig_height)

dev.off()

# sgc: spinglass.community
V(largest_comp_undirected)$membership <- sgc$membership

node_color <- case_when(
  V(largest_comp_undirected)$membership == "1" ~ "orange",
  V(largest_comp_undirected)$membership == "2" ~ "pink",
  V(largest_comp_undirected)$membership == "3" ~ "skyblue",
  V(largest_comp_undirected)$membership == "4" ~ "yellow"
)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right))
plot(largest_comp_undirected, vertex.size=14, 
     edge.arrow.width = 0.1, 
     edge.arrow.size = 0.1,
     vertex.color = node_color,
     vertex.shape = node_shape,
     layout = my_layout
)


dev.print(device = jpeg,
          filename = "spinglass_community.jpg",
          width = fig_width,
          height = fig_height)

dev.off()

# wtc: walktrap.community
V(largest_comp_undirected)$membership <- wtc$membership

node_color <- case_when(
  V(largest_comp_undirected)$membership == "1" ~ "orange",
  V(largest_comp_undirected)$membership == "2" ~ "pink",
  V(largest_comp_undirected)$membership == "3" ~ "skyblue",
  V(largest_comp_undirected)$membership == "4" ~ "yellow"
)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right))
plot(largest_comp_undirected, vertex.size=14, 
     edge.arrow.width = 0.1, 
     edge.arrow.size = 0.1,
     vertex.color = node_color,
     vertex.shape = node_shape,
     layout = my_layout
)


dev.print(device = jpeg,
          filename = "walktrap_community.jpg",
          width = fig_width,
          height = fig_height)

dev.off()


# louvain: cluster_louvain
V(largest_comp_undirected)$membership <- louvain$membership

node_color <- case_when(
  V(largest_comp_undirected)$membership == "1" ~ "orange",
  V(largest_comp_undirected)$membership == "2" ~ "pink",
  V(largest_comp_undirected)$membership == "3" ~ "skyblue",
  V(largest_comp_undirected)$membership == "4" ~ "yellow"
)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right))
plot(largest_comp_undirected, vertex.size=14, 
     edge.arrow.width = 0.1, 
     edge.arrow.size = 0.1,
     vertex.color = node_color,
     vertex.shape = node_shape,
     layout = my_layout
)


dev.print(device = jpeg,
          filename = "cluster_louvain.jpg",
          width = fig_width,
          height = fig_height)

dev.off()

