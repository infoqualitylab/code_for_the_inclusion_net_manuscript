#
# R1_salt_community_detection.R
#
# author: Yuanxi Fu
#
# Description: this file is responsible for generating Figure 3 of the manuscript
# Fu, Y., Clarke, C. V., Van Moer, M., & Schneider, J. (2022). 
# Exploring Evidence Selection with the Inclusion Network. MetaArXiv. 
# https://doi.org/10.31222/osf.io/zh9vp 

# Where to Find the data
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

# rank of the last SRR in series 1: #1, #2, #3
s1_rank <- V(G_salt)[which(V(G_salt)$name == '3')]$temporal_seq_rank

# rank of the last SRR om seroes 2: #5, #6, #12
s2_rank <- V(G_salt)[which(V(G_salt)$name == '12')]$temporal_seq_rank

# figure b
set.seed(42)
my_layout <- igraph::layout_with_fr(G_salt)

# basic figure config
fig_width = 800
fig_height = 800
fig_bottom = 0.2
fig_left = 0.2
fig_right = 0.2
fig_top = 2.5

# set node shape
node_shape <- case_when(
  
  V(G_salt)$node_type == "Systematic Review Report" ~ "square",
  V(G_salt)$node_type == "Primary Study Report" ~ 'circle'
  
)

# set node color
node_color <- case_when(
  V(G_salt)$temporal_seq_rank <= s1_rank & V(G_salt)$node_type == 'Primary Study Report' ~ "#E69F00",
  V(G_salt)$name == "3" ~ 'red',
  V(G_salt)$name == "1" ~ 'red',
  V(G_salt)$name == "2" ~ 'red',
  V(G_salt)$node_type == 'Systematic Review Report' ~ 'white',
  TRUE ~ "#999999"
)

node_label <- c()
node_size <- c()
node_type_temp <- V(G_salt)$node_type
node_name_temp <- V(G_salt)$name

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

plot(G_salt, vertex.size=node_size, 
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

title("(a) Target SRRs: #1, #2, #3",cex.main=2.5)

dev.print(device = png,
          filename = "salt_network_a.png",
          width = fig_width,
          height = fig_height)

dev.off()

# figure b
# basic figure config
fig_width = 800
fig_height = 800
fig_bottom = 0.2
fig_left = 0.2
fig_right = 0.2
fig_top = 2.5

# set node shape
node_shape <- case_when(
  
  V(G_salt)$node_type == "Systematic Review Report" ~ "square",
  V(G_salt)$node_type == "Primary Study Report" ~ 'circle'
  
)

# set node color
node_color <- case_when(
  V(G_salt)$temporal_seq_rank <= s2_rank & V(G_salt)$node_type == 'Primary Study Report' ~ "#E69F00",
  V(G_salt)$name == "5" ~ 'red',
  V(G_salt)$name == "6" ~ 'red',
  V(G_salt)$name == "12" ~ 'red',
  V(G_salt)$node_type == 'Systematic Review Report' ~ 'white',
  TRUE ~ "#999999"
)

node_label <- c()
node_size <- c()
node_type_temp <- V(G_salt)$node_type
node_name_temp <- V(G_salt)$name

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

plot(G_salt, vertex.size=node_size, 
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

title("(b) Target SRRs: #5, #6, #12",cex.main=2.5)

dev.print(device = png,
          filename = "salt_network_b.png",
          width = fig_width,
          height = fig_height)

dev.off()
