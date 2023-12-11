#
# R1_adjusted_JS_ExRx.R
#
# Author: Yuanxi Fu
# Description: this file is responsible of generating Table 2(a) of the manuscript: 
# Fu, Y., Clarke, C. V., Van Moer, M., & Schneider, J. (2024). 
# Exploring Evidence Selection with the Inclusion Network. Quantitative Science Studies
# MetaArXiv: https://doi.org/10.31222/osf.io/zh9vp 
#
#
# Where to Find the data
#
# ExRx: Clarke, Caitlin; Lischwe Mueller, Natalie; Joshi, Manasi Ballal; Fu, Yuanxi; Schneider, Jodi (2023): 
# The Inclusion Network of 27 Review Articles Published between 2013-2018 
# Investigating the Relationship Between Physical Activity and Depressive Symptoms. 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-4614455_V4
#


rm(list = ls())
source("final_functions.R")


######################
##      ExRx        ##
######################
edge_list_file_path <- 'data/ExRx/inclusion_net_edges.csv'
report_list_file_path <- 'data/ExRx/article_list.csv'
srr_search_date_file_path <- 'data/ExRx/review_article_details.csv'

edge_list <- make_edge_list_exrx(edge_list_file_path)
attr_list <- make_attr_list_exrx(report_list_file_path,
                                 srr_search_date_file_path)

# create the graph of salt controversy
G <- make_graph(attr_list = attr_list,
                     edge_list = edge_list)


# produce the adjusted jaccard similarity dataframe for the ExRx inclusion network
adj_js_df <- compute_adj_js_df(G)

# produce the regular jaccard similarity dataframe for the ExRx inclusion network
reg_js_df <- compute_reg_js_df(G)


# create a dataframe of adjusted JS ranked from low to high
adj_js_srr2 <- adj_js_df %>% 
  filter(adj_js_df$srr_1_name == "2" | adj_js_df$srr_2_name == "2" ) %>%
  arrange(adjusted_js)

# exchange the position of srr_1_name and srr_2_name if srr_1_name is not "2"
idx_list <- which(adj_js_srr2$srr_1_name != "2")

for(idx in idx_list){
  
  adj_js_srr2[idx, 2] = adj_js_srr2[idx, 1]
  adj_js_srr2[idx, 1] = "2"
     
}

# create a dataframe of regular JS ranked according to the adjusted js
reg_js_srr2 <- reg_js_df %>% 
  filter(reg_js_df$srr_1_name == "2" | reg_js_df$srr_2_name == "2" ) %>%
  arrange(reg_js)

# exchange the position of srr_1_name and srr_2_name if srr_1_name is not "2"
idx_list <- which(reg_js_srr2$srr_1_name != "2")

for(idx in idx_list){
  
  reg_js_srr2[idx, 2] = reg_js_srr2[idx, 1]
  reg_js_srr2[idx, 1] = "2"
  
}
  

# produce the merged dataframe with difference
both_js_srr2 <- dplyr::inner_join(x = dplyr::select(adj_js_srr2,srr_2_name, adjusted_js), 
                                  y = dplyr::select(reg_js_srr2,srr_2_name, reg_js),
                                  by= 'srr_2_name')

both_js_srr2 <- both_js_srr2 %>%
  dplyr:: mutate(fold_change = round(adjusted_js/reg_js, 2))


both_js_srr2$srr_2_name <- as.integer(both_js_srr2$srr_2_name)

both_js_srr2 <- both_js_srr2 %>% 
  dplyr::left_join(x = both_js_srr2,
                   y = dplyr::select(attr_list, article_id, temporal_seq_date_parsed),
                   by = join_by(srr_2_name == article_id))


both_js_srr2 <- both_js_srr2 %>% dplyr::mutate(search_diff_in_month = difftime(temporal_seq_date_parsed, '2012-07-15', units = 'days')/30)

both_js_srr2$search_diff_in_month <- round(as.numeric(both_js_srr2$search_diff_in_month),0)


##############
##Table 2(a)##
##############
both_js_srr2 <- both_js_srr2 %>% select(srr_2_name, search_diff_in_month, adjusted_js,
                                        reg_js, fold_change)

# investigate the negative decrease in between SRR #2 and SRR #8
edge_list_temp <- edge_list %>% filter(from %in% c(2,8))

edge_list_temp <- dplyr::left_join(x=edge_list_temp,
                                   y=dplyr::select(attr_list, article_id, temporal_seq_rank),
                                   by = join_by(from == article_id))

edge_list_temp <- dplyr::rename(edge_list_temp, from_rank = 'temporal_seq_rank')

edge_list_temp <- dplyr::left_join(x=edge_list_temp,
                                   y=dplyr::select(attr_list, article_id, temporal_seq_rank),
                                   by = join_by(to == article_id))
edge_list_temp <- dplyr::rename(edge_list_temp, to_rank = 'temporal_seq_rank')


