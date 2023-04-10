#
# R1_time_series_individual_ExRx.R
#
#
# ExRx: Clarke, Caitlin; Lischwe Mueller, Natalie; Joshi, Manasi Ballal; Fu, Yuanxi; Schneider, Jodi (2022): 
# The Inclusion Network of 27 Review Articles Published between 2013-2018 
# Investigating the Relationship Between Physical Activity and Depressive Symptoms. 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-4614455_V1

rm(list = ls())

source("R1_functions.R")

raw_edge_list_file_exrx <- "data/ExRx/inclusion_net_edges.csv"
raw_attr_list_file_exrx <- "data/ExRx/article_list.csv"
raw_search_date_file_exrx <- "data/ExRx/review_article_details.csv"

G_exrx <- make_exrx_graph(raw_edge_list_file_exrx, 
                          raw_attr_list_file_exrx, 
                          raw_search_date_file_exrx)

# produce the adjusted jaccard similarity dataframe for the ExRx inclusion network
adj_js_df <- compute_adj_js_df(G_exrx)

rows_to_keep <- which(as.integer(adj_js_df$srr_1_name) 
                      < as.integer(adj_js_df$srr_2_name))

adj_js_df <- adj_js_df[rows_to_keep,]

# create a dataframe to store srr search date
srr_search_date <- read.csv(file = raw_search_date_file_exrx) %>%
  select(article_id, search_year, search_month) %>%
  mutate(date = lubridate::make_date(search_year, search_month)) %>%
  arrange(date) %>% mutate(rank = rank(date))

date_vector <- unique(srr_search_date$date)
date_vector_for_df <- c()
avg_adj_js_vector_for_df <- c()
srr_name_vector_for_df <- c()

for (ID in srr_search_date$article_id) {
  
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
      srr_search_date %>% select(article_id, date, rank),
      by.x = 'srr_2_name',
      by.y = 'article_id'
    ) %>% arrange(rank)
  
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
       ylim = c(-0.05,0.45),
       xlim = c(lubridate::as_date('2012-01-01'), lubridate::as_date('2018-06-01')))
  abline(v=srr_search_date[srr_search_date$article_id == ID,]$date, lty=2, lwd=1, col='red')


  date_vector_for_df <- c(date_vector_for_df, date_vector)
  srr_name_vector_for_df <- c(srr_name_vector_for_df, rep(ID, length(date_vector)))
  avg_adj_js_vector_for_df <- c(avg_adj_js_vector_for_df, avg_js_vector)
  
}