#
# salt_community_detection.R
#
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
source("R1_functions.R")

# create the network
salt_edge_list_file_path <- 'data/salt/inclusion_net_edges.csv'
salt_report_list_file_path <- 'data/salt/report_list.csv'
salt_srr_search_date_file_path <- 'data/salt/systematic_review_inclusion_criteria.csv'

edge_list_salt <- make_edge_list_salt(salt_edge_list_file_path)
attr_list_salt <- make_attr_list_salt(salt_report_list_file_path,
                                      salt_srr_search_date_file_path)

# create the graph of salt controversy
G_salt <- make_graph(attr_list = attr_list_salt,
                     edge_list = edge_list_salt)

# select the connected component
comp_list <- components(G_salt)
largest_component_id <- which.max(comp_list$csize)
largest_component_vids <- V(G_salt)[comp_list$membership == largest_component_id]
largest_comp <- igraph::induced_subgraph(G_salt, largest_component_vids)


# make the first figure
# node color indicate study designs
set.seed(42)
my_layout <- igraph::layout.fruchterman.reingold(largest_comp)

fig_width = 600
fig_height = 600
fig_bottom = 0.2
fig_left = 0.2
fig_right = 0.2
fig_top = 0.2

node_color <- case_when(
  V(largest_comp)$study_design == "Randomized Controlled Trial" ~ "orange",
  V(largest_comp)$study_design == "Cohort Study" ~ "pink",
  V(largest_comp)$study_design == "Systematic Review" ~ "skyblue",
  V(largest_comp)$study_design == "Case Control Study" ~ "yellow"
)

node_shape <- case_when(
  
  V(largest_comp)$node_type == "Systematic Review Report" ~ "square",
  V(largest_comp)$node_type == "Primary Study Report" ~ 'circle'
  
)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right), bg=NA)

plot(largest_comp, vertex.size=10, 
     edge.arrow.width = 0.5, 
     edge.arrow.size = 0.5,
     edge.color = 'black',
     vertex.color = node_color,
     vertex.shape = node_shape,
     layout = my_layout
)

dev.print(device = png,
          filename = "figure3.png",
          width = fig_width,
          height = fig_height)

dev.off()

# edge betweenness community 
largest_comp_undirected <- as.undirected(largest_comp)

btw <- igraph::cluster_edge_betweenness(largest_comp_undirected)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right), bg=NA)

plot(btw, 
     largest_comp_undirected, 
     layout = my_layout, 
     vertex.shape = node_shape,
     vertex.color = node_color
     )

dev.print(device = png,
          filename = "figure3_communities_edge_betweenness.png",
          width = fig_width,
          height = fig_height
          )

dev.off()


# infomap community
infomap <- igraph::infomap.community(largest_comp_undirected)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right), bg=NA)

plot(infomap, 
     largest_comp_undirected, 
     layout = my_layout, 
     vertex.shape = node_shape,
     vertex.color = node_color
)

dev.print(device = png,
          filename = "figure3_communities_infomap.png",
          width = fig_width,
          height = fig_height
)

dev.off()


