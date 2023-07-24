#
# R1_time_series_exrx.R
#
# author: Yuanxi Fu
#
# Where to Find the data
#
# ExRx: Clarke, Caitlin; Lischwe Mueller, Natalie; Joshi, Manasi Ballal; Fu, Yuanxi; Schneider, Jodi (2022): 
# The Inclusion Network of 27 Review Articles Published between 2013-2018 
# Investigating the Relationship Between Physical Activity and Depressive Symptoms. 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-4614455_V3
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
my_layout <- igraph::layout.fruchterman.reingold(G_exrx)

fig_width = 600
fig_height = 600
fig_bottom = 0.2
fig_left = 0.2
fig_right = 0.2
fig_top = 0.2

group_1 <- c("2", "9", "12", "16", "21")
group_2 <- c("1", "5", "6", "8","11", "15", "28")
group_3 <- c("3", "4", "10", "13", "14", "17", "18", "19", "20",
             "22", "23", "24", "25", "26", "27", "28")


node_shape <- case_when(
  
  V(G_exrx)$node_type == "Systematic Review Report" ~ "square",
  V(G_exrx)$node_type == "Primary Study Report" ~ 'circle'
  
)

node_color <- case_when(
  V(G_exrx)$name %in% group_1 ~ "#0072B2",
  V(G_exrx)$name %in% group_2 ~ "#E69F00",
  V(G_exrx)$name %in% group_3 ~ "#F0E442",
  TRUE ~ "#999999"
)

par(mar = c(fig_bottom, fig_left, fig_top, fig_right), bg=NA)

plot(G_exrx, vertex.size=5, 
     edge.arrow.width = 0.2, 
     edge.arrow.size = 0.2,
     edge.color = 'black',
     vertex.color = node_color,
     vertex.shape = node_shape,
     layout = my_layout,
     vertex.label=NA
)

dev.print(device = png,
          filename = "ExRx_groups.png",
          width = fig_width,
          height = fig_height)

dev.off()

