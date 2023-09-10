#
# R1_ExRx_compute_percentage.R
#
# Author: Yuanxi Fu
# Description: this file is responsible of computing the fraction 
# of ExRx PSRs in a set that were published after the search date of
# a particular ExRx SRR: 
# Fu, Y., Clarke, C. V., Van Moer, M., & Schneider, J. (2022). 
# Exploring Evidence Selection with the Inclusion Network. 
# MetaArXiv. https://doi.org/10.31222/osf.io/zh9vp 

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

# Sentence 1
# The weakly connected component on the top (centered on SRR #23) 
# included many PSRs 
# published after the search date of SRR #2.

# get the rank order of SRR #2
rank_srr_2 <- igraph::vertex_attr(G, 'temporal_seq_rank', index = V(G)[V(G)$name == '2'])

total_num <-  length(neighbors(G, V(G)[V(G)$name == '23'], mode = 'out'))

post_num <- 0

for (node_x in neighbors(G, v=V(G)[V(G)$name == '23'], mode = 'out')){
  
  rank_node_x <- igraph::vertex_attr(G, 'temporal_seq_rank', index = node_x)
  
  if (rank_node_x > rank_srr_2){
    
    post_num <- post_num + 1
  
    }

}

# percentage of PSRs included in SRR #23 that are published 
# after the search date of SRR #2
post_num/total_num # 86.2%

# Sentence 2
# The weakly connected component in at the lower right corner of Figure 2 
# also included significant numbers of PSRs published after 
# the search date of SRR #2. 

node_list <- union(neighbors(G, V(G)[V(G)$name == '26'], mode = 'out'),
                   neighbors(G, V(G)[V(G)$name == '3'], mode = 'out'))

total_num <- length(node_list)

post_num <- 0

for (node_x in node_list){
  
  rank_node_x <- igraph::vertex_attr(G, 'temporal_seq_rank', index = node_x)
  
  if (rank_node_x > rank_srr_2){
    
    post_num <- post_num + 1
    
  }
  
}

# percentage of PSRs included in SRR #26 and SRR #3 that are published 
# after the search date of SRR #2
post_num/total_num # 76.9%
