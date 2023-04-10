#
# R1_time_series_population_salt.R
#
#
# author: Yuanxi Fu
#
# Where to Find the data
#
# Salt: Fu, Yuanxi; Hsiao, Tzu-Kun; Joshi, Manasi Ballal (2022): 
# The Salt Controversy Systematic Review Reports and Primary Study Reports Network Dataset . 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-6128763_V2

rm(list = ls())

source("R1_functions.R")

##############################
## Create Times Series Salt ##
##############################
# set input files: Salt Controversy
raw_edge_list_file_salt <- "data/salt/inclusion_net_edges.csv"
raw_attr_list_file_salt <- "data/salt/report_list.csv"
raw_search_date_file_salt <- "data/salt/systematic_review_inclusion_criteria.csv"

G_salt <- create_salt_graph(raw_edge_list_file_salt, 
                            raw_attr_list_file_salt, 
                            raw_search_date_file_salt)


# produce the adjusted jaccard similarity dataframe for the salt controversy inclusion network
adj_js_df_salt <- compute_adj_js_df(G_salt)
rows_to_keep <- which(as.integer(adj_js_df_salt$srr_1_name) 
                      < as.integer(adj_js_df_salt$srr_2_name))

adj_js_df_salt[rows_to_keep,]

srr_search_date <- read.csv(file=raw_search_date_file_salt) %>% 
  select(ID, last_search_year, last_search_month) %>%
  mutate(date=lubridate::make_date(last_search_year, last_search_month)) %>% arrange(date)

srr_search_date <- srr_search_date %>% mutate(rank=rank(date))

# create vectors for the nex step of computation
# srr_vector <- srr_search_date$article_id
search_date_rank_vector <- unique(rank(srr_search_date$date))
search_date_vector <- unique(srr_search_date$date)

# compute adjusted JS average
srr_group_vector <- c()
avg_js_vector <- c()

for (i in search_date_rank_vector[2:length(search_date_rank_vector)]){
  
  srr_group <- srr_search_date %>% filter(rank<=i) %>% select(ID)
  
  avg_js <- adj_js_df_salt %>% 
    filter(srr_1_name %in% srr_group$ID, 
           srr_2_name %in% srr_group$ID) %>%
    summarise(mean=mean(adjusted_js))
  
  avg_js_vector <- c(avg_js_vector, avg_js$mean)
  
}

# artificially adding the first data point: (1.0, 2000-07-01)
avg_js_vector <- c(1.0, avg_js_vector)

# create time series df
ts_df <- data.table(search_date = search_date_vector,
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
