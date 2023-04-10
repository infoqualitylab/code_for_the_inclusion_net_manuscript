#
# R1_time_series_individual_salt.R
#
# Where to find data
# Salt: Fu, Yuanxi; Hsiao, Tzu-Kun; Joshi, Manasi Ballal (2022):
# The Salt Controversy Systematic Review Reports and Primary Study Reports Network Dataset .
# University of Illinois at Urbana-Champaign.
# https://doi.org/10.13012/B2IDB-6128763_V2

rm(list = ls())
source("R1_functions.R")

# set input files: Salt Controversy
raw_edge_list_file_salt <- "data/salt/inclusion_net_edges.csv"
raw_attr_list_file_salt <- "data/salt/report_list.csv"
raw_search_date_file_salt <-
  "data/salt/systematic_review_inclusion_criteria.csv"

G_salt <- create_salt_graph(raw_edge_list_file_salt,
                            raw_attr_list_file_salt,
                            raw_search_date_file_salt)

# produce the adjusted jaccard similarity dataframe for
# the salt controversy inclusion network
adj_js_df <- compute_adj_js_df(G_salt)
rows_to_keep <- which(as.integer(adj_js_df$srr_1_name)
                      < as.integer(adj_js_df$srr_2_name))
adj_js_df <- adj_js_df[rows_to_keep, ]

# create a dataframe to store srr search date
srr_search_date <- read.csv(file = raw_search_date_file_salt) %>%
  select(ID, last_search_year, last_search_month) %>%
  mutate(date = lubridate::make_date(last_search_year, last_search_month)) %>%
  arrange(date) %>% mutate(rank = rank(date))

# avg_js_vector <- c()
date_vector <- unique(srr_search_date$date)

for (ID in srr_search_date$ID) {
  
  # create a sub dataframe
  sub_adj_js_df <-
    adj_js_df %>% filter(srr_1_name == ID | srr_2_name == ID)
  
  # switch srr_1_name and srr_2_name if srr_1_name != ID
  # make the following code easier to write
  for (i in 1:nrow(sub_adj_js_df)) {
    if (sub_adj_js_df[i,]$srr_1_name != ID) {
      temp <- sub_adj_js_df[i,]$srr_1_name
      sub_adj_js_df[i,]$srr_1_name <- ID
      sub_adj_js_df[i,]$srr_2_name <- temp
      
    }
  }
  
  # add date and rank
  sub_adj_js_df <-
    merge(
      sub_adj_js_df,
      srr_search_date %>% select(ID, date, rank),
      by.x = 'srr_2_name',
      by.y = 'ID'
    ) %>% arrange(rank)
  
  # rank_vector <- unique(sub_adj_js_df$rank)
  # date_vector <- unique(sub_adj_js_df$date)
  avg_js_vector <- c()
  
  for (d in date_vector) {
    avg_js <- sub_adj_js_df %>%
      filter(date <= d) %>%
      summarise(mean = mean(adjusted_js))
    
    avg_js_vector <- c(avg_js_vector, avg_js$mean)
    
  }
  
  plot(date_vector, avg_js_vector, 
       xlab = stringr::str_c('SRR #', ID), 
       ylab = '', 
       ylim = c(-0.1,1.1),
       xlim = c(lubridate::as_date('1999-01-01'), lubridate::as_date('2016-01-01')))
  abline(v=srr_search_date[srr_search_date$ID == ID,]$date, lty=2, lwd=1, col='red')
  
}
