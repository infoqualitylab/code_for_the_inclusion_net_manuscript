#
# final_salt_community_detection.R
#
# author: Yuanxi Fu
#
# Description: this file is responsible for generating Figure 5 of the manuscript
# Fu, Y., Clarke, C. V., Van Moer, M., & Schneider, J. (2024). 
# Exploring Evidence Selection with the Inclusion Network. Quantitative Science Studies
# MetaArXiv: https://doi.org/10.31222/osf.io/zh9vp

# Description: this file is also responsible for generating figures in the Table S2
# of the supplementary material of the manuscript
# Fu, Y., Clarke, C. V., Van Moer, M., & Schneider, J. (2024). 
# Exploring Evidence Selection with the Inclusion Network. Quantitative Science Studies
# MetaArXiv: https://doi.org/10.31222/osf.io/zh9vp
# Where to Find the data
#
# Salt: Fu, Yuanxi; Hsiao, Tzu-Kun; Joshi, Manasi Ballal (2023): 
# The Salt Controversy Systematic Review Reports and Primary Study Reports Network Dataset . 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-6128763_V3

rm(list = ls())
source("final_functions.R")

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
  V(largest_comp)$study_design == "Randomized Controlled Trial" ~ "#E69F00",
  V(largest_comp)$study_design == "Cohort Study" ~ "#D55E00",
  V(largest_comp)$study_design == "Systematic Review" ~ "#56B4E9",
  V(largest_comp)$study_design == "Case Control Study" ~ "#F0E442"
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
          filename = "salt_controversy_by_study_design.png",
          width = fig_width,
          height = fig_height)

dev.off()

# btw edge betweenness community 
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
          filename = "communities_btw.png",
          width = fig_width,
          height = fig_height
          )

dev.off()


# imfomap: infomap community
infomap <- igraph::infomap.community(largest_comp_undirected)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right), bg=NA)

plot(infomap, 
     largest_comp_undirected, 
     layout = my_layout, 
     vertex.shape = node_shape,
     vertex.color = node_color
)

dev.print(device = png,
          filename = "communities_infomap.png",
          width = fig_width,
          height = fig_height
)

dev.off()

# cluster_leading_eigen
cle <- igraph::cluster_leading_eigen(largest_comp_undirected)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right), bg=NA)

plot(cle, 
     largest_comp_undirected, 
     layout = my_layout, 
     vertex.shape = node_shape
)

dev.print(device = png,
          filename = "communities_cluster_leading_eigen.png",
          width = fig_width,
          height = fig_height
)

dev.off()

# lpc: label.propagation.community
lps <- igraph::label.propagation.community(largest_comp_undirected)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right), bg=NA)

plot(lps, 
     largest_comp_undirected, 
     layout = my_layout, 
     vertex.shape = node_shape
)


dev.print(device = png,
          filename = "communities_lpc.png",
          width = fig_width,
          height = fig_height
)

dev.off()


# mlc: multilevel.community
mlc <- igraph::multilevel.community(largest_comp_undirected)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right), bg=NA)

plot(mlc, 
     largest_comp_undirected, 
     layout = my_layout, 
     vertex.shape = node_shape
)


dev.print(device = png,
          filename = "communities_mlc.png",
          width = fig_width,
          height = fig_height
)

dev.off()


# sgc: spinglass.community
sgc <- igraph::spinglass.community(largest_comp_undirected)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right), bg=NA)

plot(sgc, 
     largest_comp_undirected, 
     layout = my_layout, 
     vertex.shape = node_shape
)


dev.print(device = png,
          filename = "communities_sgc.png",
          width = fig_width,
          height = fig_height
)

dev.off()


# wtc: walktrap.community
wtc <- igraph::walktrap.community(largest_comp_undirected)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right), bg=NA)

plot(wtc, 
     largest_comp_undirected, 
     layout = my_layout, 
     vertex.shape = node_shape
)


dev.print(device = png,
          filename = "communities_wtc.png",
          width = fig_width,
          height = fig_height
)

dev.off()



# louvain: cluster_louvain
louvain <- igraph::cluster_louvain(largest_comp_undirected)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right), bg=NA)

plot(louvain, 
     largest_comp_undirected, 
     layout = my_layout, 
     vertex.shape = node_shape
)


dev.print(device = png,
          filename = "communities_louvain.png",
          width = fig_width,
          height = fig_height
)

dev.off()

# fastgreedy: cluster_fast_greedy
fastgreedy <- igraph::cluster_fast_greedy(largest_comp_undirected)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right), bg=NA)

plot(fastgreedy, 
     largest_comp_undirected, 
     layout = my_layout, 
     vertex.shape = node_shape
)


dev.print(device = png,
          filename = "communities_fastgreedy.png",
          width = fig_width,
          height = fig_height
)

dev.off()

