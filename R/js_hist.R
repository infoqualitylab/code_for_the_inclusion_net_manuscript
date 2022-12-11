# "js_hist.R"

# This script produces figure 4 of manuscript "Exploring Evidence Selection with the Inclusion Network"
# by Yuanxi Fu, Caitlin Vitosky Clarke, Mark Van Moer, and Jodi Schneider

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

source("functions.R")

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

# produce the jaccard similarity dataframe for the ExRx inclusion network
js_df_exrx <- compute_js_df(G_exrx)

# produce the jaccard similarity dataframe for the salt controversy inclusion network
js_df_salt <- compute_js_df(G_salt)

js_df_salt$dataset = rep("Salt", nrow(js_df_salt))
js_df_exrx$dataset = rep("ExRx", nrow(js_df_exrx))

combdat <- rbind(js_df_exrx, js_df_salt)

ggplot(data = combdat, aes(x = jaccard)) +
  geom_histogram(binwidth = 0.05, fill = 'grey', color = 'black') +
  facet_wrap(~dataset) +
  scale_x_continuous(name = "Jaccard Similarity") +
  theme_bw() +
  theme(strip.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0, unit = 'pt'),
                                    size = 18),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0, unit = 'pt'),
                                    size = 18))

mean(js_df_exrx$jaccard) # 0.04725433
mean(js_df_salt$jaccard) # 0.253034

median(js_df_exrx$jaccard) # 0
median(js_df_salt$jaccard) # 0.2083333

sqrt(var(js_df_exrx$jaccard)) # 0.08257572
sqrt(var(js_df_salt$jaccard)) # 0.2891774

