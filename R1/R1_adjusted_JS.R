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
# https://doi.org/10.13012/B2IDB-4614455_V1
#
# Salt: Fu, Yuanxi; Hsiao, Tzu-Kun; Joshi, Manasi Ballal (2022): 
# The Salt Controversy Systematic Review Reports and Primary Study Reports Network Dataset . 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-6128763_V2

rm(list = ls())

source("R1_functions.R")

# construct the two inclusion network
# set input files: ExRx
raw_edge_list_file_exrx <- "data/ExRx/inclusion_net_edges.csv"
raw_attr_list_file_exrx <- "data/ExRx/article_list.csv"
raw_search_date_file_exrx <- "data/ExRx/review_article_details.csv"

G_exrx <- make_exrx_graph(raw_edge_list_file_exrx, 
                          raw_attr_list_file_exrx, 
                          raw_search_date_file_exrx)

# set input files: Salt Controversy
raw_edge_list_file_salt <- "data/salt/inclusion_net_edges.csv"
raw_attr_list_file_salt <- "data/salt/report_list.csv"
raw_search_date_file_salt <- "data/salt/systematic_review_inclusion_criteria.csv"

G_salt <- create_salt_graph(raw_edge_list_file_salt, 
                            raw_attr_list_file_salt, 
                            raw_search_date_file_salt)


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

mean(adj_js_df_exrx$adjusted_js) # 0.05184529
mean(adj_js_df_salt$adjusted_js) # 0.3243073

median(adj_js_df_exrx$adjusted_js) # 0
median(adj_js_df_salt$adjusted_js) # 0.25

sqrt(var(adj_js_df_exrx$adjusted_js)) # 0.09174764
sqrt(var(adj_js_df_salt$adjusted_js)) # 0.3608035




