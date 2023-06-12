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
salt_report_list_file_path <- 'data/salt/report_list.csv'
salt_srr_search_date_file_path <- 'data/salt/systematic_review_inclusion_criteria.csv'

edge_list_salt <- make_edge_list_salt(salt_edge_list_file_path)
attr_list_salt <- make_attr_list_salt(salt_report_list_file_path,
                                      salt_srr_search_date_file_path)

# create the graph of salt controversy
G_salt <- make_graph(attr_list = attr_list_salt,
                     edge_list = edge_list_salt)

# produce the jaccard similarity dataframe for the salt controversy inclusion network
adj_js_df_salt <- compute_adj_js_df(G_salt)

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

# produce the jaccard similarity dataframe for the salt controversy inclusion network
adj_js_df_exrx <- compute_adj_js_df(G_exrx)

# produce the adjusted jaccard similarity dataframe for the ExRx inclusion network
adj_js_df_exrx <- compute_adj_js_df(G_exrx)

######################
##  visualization   ##
######################

# Visualization
# produce the adjusted jaccard similarity dataframe for the ExRx inclusion network
adj_js_df_exrx <- compute_adj_js_df(G_exrx)

# produce the jaccard similarity dataframe for the salt controversy inclusion network
adj_js_df_salt <- compute_adj_js_df(G_salt)

adj_js_df_salt$dataset = rep("Salt", nrow(adj_js_df_salt))
adj_js_df_exrx$dataset = rep("ExRx", nrow(adj_js_df_exrx))

combdat <- rbind(adj_js_df_exrx, adj_js_df_salt)

ggplot(combdat, aes(adjusted_js, group = dataset)) + 
  geom_histogram(aes(y = stat(density)), binwidth = 0.05, fill = 'grey', color = 'black') + 
  facet_wrap(~ dataset) +
  scale_x_continuous(name = "Adjusted Jaccard Similarity") +
  theme_bw() +
  theme(strip.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0, unit = 'pt'),
                                    size = 18),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0, unit = 'pt'),
                                    size = 18))

mean(adj_js_df_exrx$adjusted_js) # 0.05354639
mean(adj_js_df_salt$adjusted_js) # 0.3255227

median(adj_js_df_exrx$adjusted_js) # 0
median(adj_js_df_salt$adjusted_js) # 0.25

sqrt(var(adj_js_df_exrx$adjusted_js)) # 0.09462773
sqrt(var(adj_js_df_salt$adjusted_js)) # 0.3607024

