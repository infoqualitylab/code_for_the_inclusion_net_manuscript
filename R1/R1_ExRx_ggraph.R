#
# R1_ExRx_ggraph.R
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
G <- make_graph(attr_list = attr_list_exrx,
                     edge_list = edge_list_exrx)

G <- tidygraph::as_tbl_graph(G)

rank_srr_2 <- V(G)[V(G)$name == '2']$temporal_seq_rank

G <- G %>% activate(nodes) %>% 
  mutate(before_after = ifelse(temporal_seq_rank <= rank_srr_2, TRUE, FALSE))

comp_list <- components(G)
largest_component_id <- which.max(comp_list$csize)
largest_component_vids <- V(G)[comp_list$membership == largest_component_id]
largest_comp <- igraph::induced_subgraph(G, largest_component_vids)

# first comp
my_layout = ggraph::create_layout(graph=largest_comp, layout = 'igraph', algorithm = 'kk')

# color palette
# #0C7BDC
# R12, G123, B220
# 
# #FFC20A
# R255, G194, B10

ggraph(graph=largest_comp, layout = my_layout) + 
  geom_edge_fan(aes(alpha = after_stat(index)), show.legend = FALSE) +
  geom_node_point(aes(shape=node_type, color=before_after), 
                  size=3.5, show.legend = FALSE) +
  geom_node_label(mapping = aes(label=ifelse(node_type == 'Systematic Review Report', name, NA),
                      fontface = 'bold'),
                  show.legend = FALSE,
                  size = 4.5) +
  theme_graph(background = NULL) +
  scale_color_manual(values = c('#0C7BDC', '#FFC20A'))

second_comp_vids <- V(G)[comp_list$membership == 2]
second_comp <- igraph::induced_subgraph(G, second_comp_vids)

my_layout = ggraph::create_layout(graph=second_comp, layout = 'igraph', algorithm = 'kk')

ggraph(graph=second_comp, layout = my_layout) + 
  geom_edge_fan(aes(alpha = after_stat(index)), show.legend = FALSE) +
  geom_node_point(aes(shape=node_type, color=before_after), 
                  size=4, show.legend = FALSE) +
  geom_node_label(mapping = aes(label=ifelse(node_type == 'Systematic Review Report', name, NA),
                                fontface = 'bold'),
                  show.legend = FALSE) +
  theme_graph(background = NULL) +
  scale_color_manual(values = c('#0C7BDC', '#FFC20A'))

# ggsave(filename = 'ExRx_second_comp.png',
#        device = 'png',
#        height = 300,
#        width = 400,
#        units = 'px')
# 
# dev.off()

third_comp_vids <- V(G)[comp_list$membership == 3]
third_comp <- igraph::induced_subgraph(G, third_comp_vids)

my_layout = ggraph::create_layout(graph=third_comp, layout = 'igraph', algorithm = 'kk')
ggraph(graph=third_comp, layout = my_layout) + 
  geom_edge_fan(aes(alpha = after_stat(index)), show.legend = FALSE) +
  geom_node_point(aes(shape=node_type, color=before_after), 
                  size=4, show.legend = FALSE) +
  geom_node_label(mapping = aes(label=ifelse(node_type == 'Systematic Review Report', name, NA),
                                fontface = 'bold'),
                  show.legend = FALSE) +
  theme_graph(background = NULL) +
  scale_color_manual(values = c('#0C7BDC', '#FFC20A'))

