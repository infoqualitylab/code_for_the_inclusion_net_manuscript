#
# R1_time_series_salt.R
#
# Author: Yuanxi Fu
# Description: this file is responsible of computing the fraction 
# of salt controversy PSRs in a set that were 
# published after the search date of a particular salt controversy SRR:
# Fu, Y., Clarke, C. V., Van Moer, M., & Schneider, J. (2022). 
# Exploring Evidence Selection with the Inclusion Network. MetaArXiv. 
# https://doi.org/10.31222/osf.io/zh9vp 

# Where to Find the data
#
#
# Salt: Fu, Yuanxi; Hsiao, Tzu-Kun; Joshi, Manasi Ballal (2022): 
# The Salt Controversy Systematic Review Reports and Primary Study Reports Network Dataset . 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-6128763_V3
#


rm(list = ls())
source("R1_functions.R")

######################
## Salt Controversy ##
######################

edge_list_file_path <- 'data/salt/inclusion_net_edges.csv'
report_list_file_path <- 'data/salt/report_list.csv'
srr_search_date_file_path <- 'data/salt/systematic_review_inclusion_criteria.csv'

edge_list <- make_edge_list_salt(edge_list_file_path)
attr_list <- make_attr_list_salt(report_list_file_path,
                                 srr_search_date_file_path)

# create the graph of salt controversy
G <- make_graph(attr_list = attr_list, edge_list = edge_list)

# Sentence 1
# Furthermore, significant amount of PSRs were published after 
# SRR #1, #2, and #3 finished their search (Figure 3(a)). 
# Therefore, SRR #1, #2 and #3 are not up-to-date. 

rank_srr_3 <- igraph::vertex_attr(G, 'temporal_seq_rank', index = V(G)[V(G)$name == '3'])

total_num <- length(V(G)[V(G)$node_type == "Primary Study Report"])
post_num <- length(V(G)[V(G)$temporal_seq_rank > rank_srr_3 & V(G)$node_type == "Primary Study Report"])

# the fraction of salt controversy PSRs published after 
# the last search date of SRR #3
post_num/total_num # (72.1%)

