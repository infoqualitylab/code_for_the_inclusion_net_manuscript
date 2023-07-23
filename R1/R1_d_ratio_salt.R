# R1_d_ratio_salt.R
# Author: Yuanxi Fu
# Description: This file creates Figure S2(b) in the supplementary material 
# of the manuscript: 
# Fu, Y., Clarke, C. V., Van Moer, M., & Schneider, J. (2022). 
# Exploring Evidence Selection with the Inclusion Network. 
# MetaArXiv. https://doi.org/10.31222/osf.io/zh9vp
#

# Where to Find the data

#
# Salt: Fu, Yuanxi; Hsiao, Tzu-Kun; Joshi, Manasi Ballal (2022): 
# The Salt Controversy Systematic Review Reports and Primary Study Reports Network Dataset . 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-6128763_V3

rm(list = ls())
source("R1_functions.R")

######################
## Salt Controversy ##
######################

salt_edge_list_file_path <- 'data/salt/inclusion_net_edges.csv'
salt_report_list_file_path <- 'data/salt/report_list.csv'
salt_srr_search_date_file_path <- 'data/salt/systematic_review_inclusion_criteria.csv'

edge_list_salt <- make_edge_list_salt(salt_edge_list_file_path)
attr_list_salt <- make_attr_list_salt(salt_report_list_file_path,
                                      salt_srr_search_date_file_path)

# create the graph of salt controversy
G_salt <- make_graph(attr_list = attr_list_salt,
                     edge_list = edge_list_salt)

# create time series
search_date_list_salt <- unique(attr_list_salt[attr_list_salt$node_type == 'Systematic Review Report',]$temporal_seq_date_same_precision)
search_date_list_salt <- sort(lubridate::parse_date_time(search_date_list_salt, orders = c("m/d/Y")))
rank_list_salt <- sort(unique(attr_list_salt[attr_list_salt$node_type == 'Systematic Review Report',]$temporal_seq_rank))

# initiate the date storage list
avg_d_ratio_list <- c()
sd_d_ratio_list <- c()
max_d_ratio_list <- c()
min_d_ratio_list <- c()

# get average d_ratio
for (rank in rank_list_salt){
  
  sub_G <- igraph::induced_subgraph(
    graph = G_salt,
    vids = V(G_salt)[V(G_salt)$temporal_seq_rank <= rank]
  )
  
  edgelist <- data.frame(igraph::as_edgelist(sub_G))
  
  avg_d_ratio <- mean(compute_d_ratio(edgelist)$d_ratio)
  sd_d_ratio <- sd(compute_d_ratio(edgelist)$d_ratio)
  min_d_ratio <- min(compute_d_ratio(edgelist)$d_ratio)
  max_d_ratio <- max(compute_d_ratio(edgelist)$d_ratio)
  
  avg_d_ratio_list <- c(avg_d_ratio_list,
                        avg_d_ratio)
  sd_d_ratio_list <- c(sd_d_ratio_list,
                       sd_d_ratio)
  
  max_d_ratio_list <- c(max_d_ratio_list,
                        max_d_ratio)
  
  min_d_ratio_list <- c(min_d_ratio_list,
                        min_d_ratio)
}

avg_d_ratio_df_salt <- data.frame(
  date = search_date_list_salt,
  mean = avg_d_ratio_list,
  se = sd_d_ratio_list,
  min = min_d_ratio_list,
  max = max_d_ratio_list, 
  dataset = 'salt')

# create full dataframe instead of summary stats
d_ratio_df_salt <- data.frame()

for (rank in rank_list_salt){
  
  sub_G <- igraph::induced_subgraph(
    graph = G_salt,
    vids = V(G_salt)[V(G_salt)$temporal_seq_rank <= rank]
  )
  
  edgelist <- data.frame(igraph::as_edgelist(sub_G))
  
  df_temp <- compute_d_ratio(edgelist)
  
  date_temp <- search_date_list_salt[which(rank_list_salt == rank)]
  
  df_temp$date <- date_temp
  
  d_ratio_df_salt <- rbind(d_ratio_df_salt, df_temp)
  
}

ggplot() + 
  geom_point(data=d_ratio_df_salt, mapping = aes(x=date, y=d_ratio), 
             color='blue', shape=1, size=4) +
  geom_point(data=avg_d_ratio_df_salt, mapping=aes(x=date, y=mean)) +
  geom_line(data=avg_d_ratio_df_salt, mapping=aes(x=date, y=mean)) +
  labs(x="Year", y="Dandelion-ness Ratio") +
  scale_y_continuous(limits = c(-0.1, 1.1), breaks=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0, unit = 'pt'),
                                    size = 16),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0, unit = 'pt'),
                                    size = 16)) +
  scale_x_datetime(date_breaks = '2 years', date_labels = '%Y')
