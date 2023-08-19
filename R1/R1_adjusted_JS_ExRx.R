#
# R1_adjusted_JS_ExRx.R
#
# Author: Yuanxi Fu
# Description: this file is responsible of generating Table 1 of the manuscript: 
# Fu, Y., Clarke, C. V., Van Moer, M., & Schneider, J. (2022). 
# Exploring Evidence Selection with the Inclusion Network. 
# MetaArXiv. https://doi.org/10.31222/osf.io/zh9vp 
#
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
  dplyr:: mutate(diff = round((adjusted_js - reg_js)/adjusted_js * 100, 1))

# stat testing -- ExRx
# define two groups
# (1) SRR #2 with SRR #1, #3, #4, #5, #6, and #8 to #28
# (2) SRR #1, #3, #4, #5, #6, and #8 to #28 amongest themselves

# g_1 <- adj_js_df_exrx %>% 
#   filter(adj_js_df_exrx$srr_1_name == "2" | adj_js_df_exrx$srr_2_name == "2" ) %>%
#   select(adjusted_js)
# 
# g_1 <- g_1$adjusted_js
# 
# g_2 <- adj_js_df_exrx %>% 
#   filter(adj_js_df_exrx$srr_1_name != "2" & adj_js_df_exrx$srr_2_name != "2" ) %>%
#   select(adjusted_js)
# 
# g_2 <- g_2$adjusted_js
# 
# adj_js <- c(g_1, g_2)
# group <- factor(c(1,2))
# 
# wilcox.test(g_1, g_2, alternative = 'two.sided')
# 
# x <- c(g_1, g_2)
# length((g_1))
# length(g_2)
# g <- factor(rep(1:2, c(26, 325)))
# 
# kruskal.test(x,g)

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
  
