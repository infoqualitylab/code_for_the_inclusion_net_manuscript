#
# R1_time_series_salt.R
#
# author: Yuanxi Fu
#
# Where to Find the data
#
#
# Salt: Fu, Yuanxi; Hsiao, Tzu-Kun; Joshi, Manasi Ballal (2022): 
# The Salt Controversy Systematic Review Reports and Primary Study Reports Network Dataset . 
# University of Illinois at Urbana-Champaign. 
# https://doi.org/10.13012/B2IDB-6128763_V2
#


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

adj_js_df <- adj_js_df_salt
attr_list <- attr_list_salt

# compute time series
search_date_list <- attr_list[attr_list$node_type == 'Systematic Review Report',]$temporal_seq_date_same_precision
srr_name_list <- attr_list[attr_list$node_type == 'Systematic Review Report',]$ID
rank_list <- attr_list[attr_list$node_type == 'Systematic Review Report',]$temporal_seq_rank

# initiate vectors to store three values for the resulted time series dataframe
date_vector <- c()
srr_name_vector <- c()
avg_adj_js_vector <- c()

for (ID in srr_name_list){
  
  # create a sub dataframe that only contain pairs including ID
  sub_adj_js_df <- adj_js_df %>% filter(srr_1_name == ID | srr_2_name == ID)
  
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
      attr_list[attr_list$node_type == 'Systematic Review Report',] %>% 
        select(ID, temporal_seq_date_same_precision, temporal_seq_rank),
      by.x = 'srr_2_name',
      by.y = 'ID'
    ) %>% arrange(temporal_seq_rank)
  
  sub_avg_js_vector <- c()
  
  # loop through the rank list to compute average adj JS
  # compute average adj JS for all SRRs with rank smaller or equal to r
  # all SRRs that have completed their search when the SRR completed its search
  for (r in rank_list) {
    
    avg_js <- sub_adj_js_df %>%
      filter(temporal_seq_rank <= r) %>%
      summarise(mean = mean(adjusted_js))
    
    sub_avg_js_vector <- c(sub_avg_js_vector, avg_js$mean)
  
    }
  
  date_vector <- c(date_vector, search_date_list)
  srr_name_vector <- c(srr_name_vector, rep(ID, length(search_date_list)))
  avg_adj_js_vector <- c(avg_adj_js_vector, sub_avg_js_vector)
  
}

# data structure of the resulted dataframe
avg_adj_js_ts_df <- tibble(date = lubridate::parse_date_time(date_vector, orders = c("m/d/Y")),
                                 srr_name = srr_name_vector,
                                 avg_ad_js = avg_adj_js_vector)

# add labels for making facets in plotting
avg_adj_js_ts_df$label <- stringr::str_c("#", avg_adj_js_ts_df$srr_name)
avg_adj_js_ts_df$label <- factor(avg_adj_js_ts_df$label,
                                   levels = stringr::str_c("#", srr_name_list))

# # old code, for traces divided by search date
# avg_adj_js_ts_df <- avg_adj_js_ts_df %>% dplyr::inner_join(y= tibble(srr_name=srr_name_list, 
#                                                  search_date=search_date_list), 
#                                        by = "srr_name")

# avg_adj_js_ts_df$Color <- rep('After the last search date', nrow(avg_adj_js_ts_df))

# # add color column
# for (i in 1:nrow(avg_adj_js_ts_df)){
#   
#   if (lubridate::parse_date_time(avg_adj_js_ts_df[i,]$search_date,
#                                  orders = c("m/d/Y")) 
#       < avg_adj_js_ts_df[i,]$date) {
#     
#     avg_adj_js_ts_df[i,]$Color <- 'Before the last search date'
#   
#     }
# }

# add color column
# initiate
avg_adj_js_ts_df$Color <- rep('After the publication date', nrow(avg_adj_js_ts_df))

# add a column to indicate publication date
avg_adj_js_ts_df <- avg_adj_js_ts_df %>% dplyr::inner_join(y=attr_list%>%select(ID, pub_date), 
                                                           by=join_by(srr_name==ID))

# use the convention used before
# convert format Y-m to m/15/Y (mid of the month)
pattern <- "(^[0-9]{4})-([0-9]{1,2}$)"
replacement <- "\\2/15/\\1"
avg_adj_js_ts_df$pub_date <- stringr::str_replace_all(avg_adj_js_ts_df$pub_date, 
                                          pattern = pattern,
                                          replacement = replacement)

# add color column
for (i in 1:nrow(avg_adj_js_ts_df)){

  if (lubridate::parse_date_time(avg_adj_js_ts_df[i,]$pub_date,
                                 orders = c("m/d/Y"))
      > avg_adj_js_ts_df[i,]$date) {

    avg_adj_js_ts_df[i,]$Color <- 'Before the publication date'

    }
}

# avg_adj_js_ts_df$Color <- dplyr::case_when(
#   
#   avg_adj_js_ts_df$classification == "Before the publication date" ~ "red",
#   avg_adj_js_ts_df$classification == "After the publication date" ~ "black"
#   
# )

# corerce the order of "After the publication date" and "Before the publication date"
avg_adj_js_ts_df$Color <- factor(avg_adj_js_ts_df$Color,
                                    levels = c("Before the publication date",
                                               "After the publication date"))

# plot

ggplot(data = avg_adj_js_ts_df, 
       mapping = aes(x = as.Date(date), y = avg_ad_js, group = label)) +
  geom_line(color="black", show.legend = FALSE) +
  geom_point(mapping = aes(color=Color)) +
  facet_wrap(vars(label), ncol = 14) +
  labs(y = "Average Adjusted Jaccard Similarity", x = 'Year') +
  theme(strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face='bold'),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0, unit = 'pt'),
                                    size = 18),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0, unit = 'pt'),
                                    size = 18)) +
  scale_y_continuous(limits = c(0, 1.0)) +
  scale_x_date(date_breaks = "5 years", date_labels = '%y')

# two rows
ggplot(data = avg_adj_js_ts_df, 
       mapping = aes(x = as.Date(date), y = avg_ad_js, group = label)) +
  geom_line(color="black", show.legend = FALSE) +
  geom_point(mapping = aes(color=Color)) +
  facet_wrap(vars(label), ncol = 7) +
  labs(y = "Average Adjusted Jaccard Similarity", x = 'Year') +
  theme(strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face='bold'),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0, unit = 'pt'),
                                    size = 18),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0, unit = 'pt'),
                                    size = 18)) +
  scale_y_continuous(limits = c(0, 1.0)) +
  scale_x_date(date_breaks = "5 years", date_labels = '%y')

