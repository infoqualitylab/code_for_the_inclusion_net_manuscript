#
# R1_time_series_population_ExRx.R
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

rm(list = ls())

source("R1_functions.R")

##############################
## Create Times Series ExRx ##
##############################
raw_edge_list_file_exrx <- "data/ExRx/inclusion_net_edges.csv"
raw_attr_list_file_exrx <- "data/ExRx/article_list.csv"
raw_search_date_file_exrx <- "data/ExRx/review_article_details.csv"

G_exrx <- make_exrx_graph(raw_edge_list_file_exrx, 
                          raw_attr_list_file_exrx, 
                          raw_search_date_file_exrx)

# produce the adjusted jaccard similarity dataframe for the ExRx inclusion network
adj_js_df_exrx <- compute_adj_js_df(G_exrx)

rows_to_keep <- which(as.integer(adj_js_df_exrx$srr_1_name) 
                      < as.integer(adj_js_df_exrx$srr_2_name))

adj_js_df_exrx[rows_to_keep,]

srr_search_date <- read.csv(file=raw_search_date_file_exrx) %>% 
  select(article_id, search_year, search_month) %>%
  mutate(date=lubridate::make_date(search_year, search_month)) %>% 
  arrange(date) %>% mutate(rank=rank(date))

# create vectors for the nex step of computation
# srr_vector <- srr_search_date$article_id
search_date_rank_vector <- unique(rank(srr_search_date$date))
search_date_vector <- unique(srr_search_date$date)

# compute adjusted JS average
srr_group_vector <- c()
avg_js_vector <- c()

for (i in search_date_rank_vector[2:length(search_date_rank_vector)]){
  
  srr_group <- srr_search_date %>% filter(rank<=i) %>%
    select(article_id)
  
  avg_js <- adj_js_df_exrx %>% 
    filter(srr_1_name %in% srr_group$article_id , 
           srr_2_name %in% srr_group$article_id) %>%
    summarise(mean=mean(adjusted_js))
  
  avg_js_vector <- c(avg_js_vector, avg_js$mean)
  
}

# create time series df
ts_df <- data.table(search_date = search_date_vector[2:length(search_date_vector)],
                    avg_js = avg_js_vector)

ggplot(ts_df) +
  geom_line(mapping=aes(x=search_date, y=avg_js)) +
  geom_point(mapping=aes(x=search_date, y=avg_js), color='steelblue', size=3) +
  scale_x_date(name = 'Last Search Completion Date', breaks = '1 year', date_labels = '%Y') +
  scale_y_continuous(name = 'Cummulative Average\nAdjusted Jaccard Similarity') +
  theme_bw() +
  theme(strip.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0, unit = 'pt'),
                                    size = 18),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0, unit = 'pt'),
                                    size = 18))