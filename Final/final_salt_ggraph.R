#
# final_salt_ggraph.R
#
# author: Yuanxi Fu
#
# Description: this file is responsible for generating Figure 3 of the manuscript
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
G <- make_graph(attr_list = attr_list_salt,
                     edge_list = edge_list_salt)

G <- tidygraph::as_tbl_graph(G)

rank_srr_12 <- V(G)[V(G)$name == '12']$temporal_seq_rank

G <- G %>% activate(nodes) %>% 
  mutate(before_after = ifelse(temporal_seq_rank <= rank_srr_12, TRUE, FALSE))

set.seed(42)
my_layout = ggraph::create_layout(graph=G, layout = 'igraph', 
                                  algorithm = 'nicely')

ggraph(graph=G, layout = my_layout) + 
  geom_edge_fan(aes(alpha = after_stat(index)), show.legend = FALSE) +
  geom_node_point(aes(shape=node_type, color=before_after), 
                  size=3.5, show.legend = FALSE) +
  geom_node_label(mapping = aes(label=ifelse(node_type == 'Systematic Review Report', name, NA),
                                fontface = 'bold'),
                  show.legend = FALSE,
                  size = 4.5) +
  theme_graph(background = NULL) +
  scale_color_manual(values = c('#0C7BDC', '#FFC20A'))
