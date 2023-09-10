#
# R1_ExRx_vis.R
#
# Author: Yuanxi Fu
# 
# Description: this file is responsible for generating Figure 2 of the manuscript
# Fu, Y., Clarke, C. V., Van Moer, M., & Schneider, J. (2022). 
# Exploring Evidence Selection with the Inclusion Network. MetaArXiv. 
# https://doi.org/10.31222/osf.io/zh9vp 


# Where to Find the data
#
# ExRx: Clarke, Caitlin; Lischwe Mueller, Natalie; Joshi, Manasi Ballal; Fu, Yuanxi; Schneider, Jodi (2022): 
# The Inclusion Network of 27 Review Articles Published between 2013-2018 
# Investigating the Relationship Between Physical Activity and Depressive Symptoms. 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-4614455_V2
#
#

rm(list = ls())
source("R1_functions.R")

######################
######## ExRx ########
######################

exrx_edge_list_file_path <- 'data/ExRx/inclusion_net_edges.csv'
exrx_report_list_file_path <- 'data/ExRx/article_list.csv'
exrx_srr_search_date_file_path <- 'data/ExRx/review_article_details.csv'

edge_list_exrx <- make_edge_list_exrx(exrx_edge_list_file_path)
attr_list_exrx <- make_attr_list_exrx(exrx_report_list_file_path,
                                      exrx_srr_search_date_file_path)

# create the graph of salt controversy
G_exrx <- make_graph(attr_list = attr_list_exrx,
                     edge_list = edge_list_exrx)

set.seed(42)
my_layout <- igraph::layout_with_fr(G_exrx)
# my_layout <- igraph::layout.kamada.kawai(G_exrx)

fig_width = 1000
fig_height = 800
fig_bottom = 0.2
fig_left = 0.2
fig_right = 0.2
fig_top = 0.2

node_shape <- case_when(
  
  V(G_exrx)$node_type == "Systematic Review Report" ~ "square",
  V(G_exrx)$node_type == "Primary Study Report" ~ 'circle'
  
)

node_color <- case_when(
  V(G_exrx)$temporal_seq_rank == 204.0 ~ "red",
  V(G_exrx)$temporal_seq_rank <= 204.0 & V(G_exrx)$node_type == 'Primary Study Report' ~ "#E69F00",
  V(G_exrx)$node_type == 'Systematic Review Report' ~ 'white',
  TRUE ~ "#999999"
)

node_label <- c()
node_size <- c()
node_type_temp <- V(G_exrx)$node_type
node_name_temp <- V(G_exrx)$name

for (i in 1:length(node_type_temp)){
  
  if (node_type_temp[i] == 'Systematic Review Report'){
    
    node_label <- c(node_label, node_name_temp[i])
    node_size <- c(node_size, 8)
    
  }else{
    
    node_label <- c(node_label, NA)
    node_size <- c(node_size, 4)
    
  }
}

par(mar = c(fig_bottom, fig_left, fig_top, fig_right), bg=NA)

plot(G_exrx, vertex.size=node_size, 
     edge.arrow.width = 0.2, 
     edge.arrow.size = 0.2,
     edge.color = 'black',
     vertex.color = node_color,
     vertex.shape = node_shape,
     layout = my_layout,
     vertex.label = node_label,
     vertex.label.cex = 2,
     vertex.label.font = 2
)

dev.print(device = png,
          filename = "ExRx_network.png",
          width = fig_width,
          height = fig_height)

dev.off()

