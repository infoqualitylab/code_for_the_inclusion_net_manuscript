#
# R1_adjusted_JS.R
#

# author: Yuanxi Fu
#
# Where to Find the data
#
# ExRx: Clarke, Caitlin; Lischwe Mueller, Natalie; Joshi, Manasi Ballal; Fu, Yuanxi; Schneider, Jodi (2022): 
# The Inclusion Network of 27 Review Articles Published between 2013-2018 
# Investigating the Relationship Between Physical Activity and Depressive Symptoms. 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-4614455_V2
#
# Salt: Fu, Yuanxi; Hsiao, Tzu-Kun; Joshi, Manasi Ballal (2022): 
# The Salt Controversy Systematic Review Reports and Primary Study Reports Network Dataset . 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-6128763_V2

rm(list = ls())
source("R1_functions.R")

######################
## Salt Controversy ##
######################

salt_edge_list_file_path <- 'data/salt/inclusion_net_edges.csv'
report_list_file_path <- 'data/salt/report_list.csv'
systematic_review_inclusion_criteria_file_path <- 'data/salt/systematic_review_inclusion_criteria.csv'

edge_list_salt <- make_edge_list_salt(salt_edge_list_file_path)
attr_list_salt <- make_attr_list_salt(report_list_file_path,
                                      systematic_review_inclusion_criteria_file_path)

# create the graph of salt controversy
G_salt <- make_graph(attr_list = attr_list_salt,
                     edge_list = edge_list_salt)

# produce the jaccard similarity dataframe for the salt controversy inclusion network
adj_js_df_salt <- compute_adj_js_df(G_salt)
