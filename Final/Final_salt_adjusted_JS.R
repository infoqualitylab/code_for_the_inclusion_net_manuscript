#
# final_salt_adjusted_JS.R
#
# Author: Yuanxi Fu
# Description: this file is responsible for generating Table 2(b) of the manuscript
# Fu, Y., Clarke, C. V., Van Moer, M., & Schneider, J. (2024). 
# Exploring Evidence Selection with the Inclusion Network. Quantitative Science Studies
# MetaArXiv: https://doi.org/10.31222/osf.io/zh9vp  

# Where to Find the data
#
#
# Salt: Fu, Yuanxi; Hsiao, Tzu-Kun; Joshi, Manasi Ballal (2023): 
# The Salt Controversy Systematic Review Reports and Primary Study Reports Network Dataset . 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-6128763_V3
#

rm(list = ls())
source("final_functions.R")

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

# produce the adjusted jaccard similarity dataframe for the ExRx inclusion network
adj_js_df <- compute_adj_js_df(G)

# produce the regular jaccard similarity dataframe for the ExRx inclusion network
reg_js_df <- compute_reg_js_df(G)

### SRR #12
# create a dataframe of adjusted JS ranked from low to high

adj_js_srr12 <- adj_js_df %>% 
  filter(adj_js_df$srr_1_name == "12" | adj_js_df$srr_2_name == "12" ) %>%
  arrange(adjusted_js)

# exchange the position of srr_1_name and srr_2_name if srr_1_name is not "2"
idx_list <- which(adj_js_srr12$srr_1_name != "12")

for(idx in idx_list){
  
  adj_js_srr12[idx, 2] = adj_js_srr12[idx, 1]
  adj_js_srr12[idx, 1] = "12"
  
}

# create a dataframe of regular JS ranked according to the adjusted js
reg_js_srr12 <- reg_js_df %>% 
  filter(reg_js_df$srr_1_name == "12" | reg_js_df$srr_2_name == "12" )

# exchange the position of srr_1_name and srr_2_name if srr_1_name is not "2"
idx_list <- which(reg_js_srr12$srr_1_name != "12")

for(idx in idx_list){
  
  reg_js_srr12[idx, 2] = reg_js_srr12[idx, 1]
  reg_js_srr12[idx, 1] = "12"
  
}


# produce the merged dataframe with difference
both_js_srr12 <- dplyr::inner_join(x = dplyr::select(adj_js_srr12,srr_2_name, adjusted_js), 
                                  y = dplyr::select(reg_js_srr12,srr_2_name, reg_js),
                                  by= 'srr_2_name')

both_js_srr12 <- both_js_srr12 %>%
  dplyr:: mutate(fold_change = round(adjusted_js/reg_js, 2))


both_js_srr12$srr_2_name <- as.integer(both_js_srr12$srr_2_name)

both_js_srr12 <- both_js_srr12 %>% 
  dplyr::left_join(x = both_js_srr12,
                   y = dplyr::select(attr_list, ID, temporal_seq_date_parsed),
                   by = join_by(srr_2_name == ID))


both_js_srr12 <- both_js_srr12 %>% dplyr::mutate(search_diff_in_month = difftime(temporal_seq_date_parsed, '2013-05-01', units = 'days')/30)

both_js_srr12$search_diff_in_month <- round(as.numeric(both_js_srr12$search_diff_in_month),0)

both_js_srr12 <- both_js_srr12 %>% select(srr_2_name, search_diff_in_month, adjusted_js,
                                        reg_js, fold_change) %>% filter(srr_2_name != '5') %>%
  filter(srr_2_name != '6')

## Table 2(b) ##
both_js_srr12[is.na(both_js_srr12$fold_change), "fold_change"] = 1
print(both_js_srr12)
