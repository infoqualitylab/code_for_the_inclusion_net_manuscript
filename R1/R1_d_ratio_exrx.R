# R1_d_ratio_exrx.R
# Author: Yuanxi Fu
# Description: This file creates Figure S2(a) in the supplementary material 
# of the manuscript: 
# Fu, Y., Clarke, C. V., Van Moer, M., & Schneider, J. (2022). 
# Exploring Evidence Selection with the Inclusion Network. 
# MetaArXiv. https://doi.org/10.31222/osf.io/zh9vp
#

# Where to Find the data
#
# ExRx: Clarke, Caitlin; Lischwe Mueller, Natalie; Joshi, Manasi Ballal; Fu, Yuanxi; Schneider, Jodi (2022): 
# The Inclusion Network of 27 Review Articles Published between 2013-2018 
# Investigating the Relationship Between Physical Activity and Depressive Symptoms. 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-4614455_V3

rm(list = ls())
source("R1_functions.R")

######################
##      ExRx        ##
######################
exrx_edge_list_file_path <- 'data/ExRx/inclusion_net_edges.csv'
exrx_report_list_file_path <- 'data/ExRx/article_list.csv'
exrx_srr_search_date_file_path <- 'data/ExRx/review_article_details.csv'

edge_list_exrx <- make_edge_list_exrx(exrx_edge_list_file_path)
attr_list_exrx <- make_attr_list_exrx(exrx_report_list_file_path,
                                      exrx_srr_search_date_file_path)

# create the graph of salt controversy
G_exrx <- make_graph(attr_list = attr_list_exrx,
                     edge_list = edge_list_exrx)


# create time series
search_date_list_exrx <- unique(attr_list_exrx[attr_list_exrx$node_type == 'Systematic Review Report',]$temporal_seq_date_same_precision)
search_date_list_exrx <- sort(lubridate::parse_date_time(search_date_list_exrx, orders = c("m/d/Y")))
rank_list_exrx <- sort(unique(attr_list_exrx[attr_list_exrx$node_type == 'Systematic Review Report',]$temporal_seq_rank))

# initiate the date storage list
avg_d_ratio_list <- c()
sd_d_ratio_list <- c()
max_d_ratio_list <- c()
min_d_ratio_list <- c()

# get average d_ratio
for (rank in rank_list_exrx){
  
  sub_G <- igraph::induced_subgraph(
    graph = G_exrx,
    vids = V(G_exrx)[V(G_exrx)$temporal_seq_rank <= rank]
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


# create full dataframe instead of summary stats
d_ratio_df_exrx <- data.frame()

for (rank in rank_list_exrx){
  
  sub_G <- igraph::induced_subgraph(
    graph = G_exrx,
    vids = V(G_exrx)[V(G_exrx)$temporal_seq_rank <= rank]
  )
  
  edgelist <- data.frame(igraph::as_edgelist(sub_G))
  
  df_temp <- compute_d_ratio(edgelist)
  
  date_temp <- search_date_list_exrx[which(rank_list_exrx == rank)]
  
  df_temp$date <- date_temp
  
  d_ratio_df_exrx <- rbind(d_ratio_df_exrx, df_temp)
  
}

avg_d_ratio_df_exrx <- data.frame(
  date = search_date_list_exrx,
  mean = avg_d_ratio_list,
  se = sd_d_ratio_list,
  min = min_d_ratio_list,
  max = max_d_ratio_list, 
  dataset = "exrx")

# plot
ggplot() + 
  geom_point(data=d_ratio_df_exrx, mapping = aes(x=date, y=d_ratio), 
             color='blue', shape=1, size=4) +
  geom_point(data=avg_d_ratio_df_exrx, mapping=aes(x=date, y=mean)) +
  geom_line(data=avg_d_ratio_df_exrx, mapping=aes(x=date, y=mean)) +
  labs(x="Year", y="Dandelion-ness Ratio") +
  scale_y_continuous(limits = c(-0.1, 1.1), breaks=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0, unit = 'pt'),
                                    size = 16),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0, unit = 'pt'),
                                    size = 16)) +
  scale_x_datetime(date_breaks = '2 years', date_labels = '%Y')
