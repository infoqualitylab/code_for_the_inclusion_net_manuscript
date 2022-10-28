rm(list = ls())

source("functions.R")

# construct the two inclusion network
# set input files: ExRx
raw_edge_list_file <- "data/ExRx/inclusion_net_edges.csv"
raw_attr_list_file <- "data/ExRx/article_list.csv"
raw_search_date_file <- "data/ExRx/review_article_details.csv"

G_exrx <- make_exrx_graph(raw_edge_list_file, 
                          raw_attr_list_file, 
                          raw_search_date_file)

# set input files: Salt Controversy
raw_edge_list_file <- "data/salt/inclusion_net_edges.csv"
raw_attr_list_file <- "data/salt/report_list.csv"
raw_search_date_file <- "data/salt/systematic_review_inclusion_criteria.csv"

G_salt <- create_salt_graph(raw_edge_list_file, 
                            raw_attr_list_file, 
                            raw_search_date_file)

#########################################
## Create Average Adjusted JS for ExRx ##
#########################################

# create a dataframe to store output --
year_list <- sort(unique(V(G_exrx)[V(G_exrx)$type == "Systematic Review Report"]$year))
srr_list <- V(G_exrx)[V(G_exrx)$type == "Systematic Review Report"]$name

# a data structure to store data
year_vector <- c()
srr_name_vector <- c()
avg_adjusted_js_vector <- c()

for (year in year_list){
  for (srr_name in srr_list){
    
    function_output <- avg_adjusted_js(year=year,
                                       srr_name = srr_name,
                                       G = G_exrx)
    
    year_vector <- c(year_vector, function_output$year)
    
    srr_name_vector <- c(srr_name_vector, function_output$srr_name)
    
    avg_adjusted_js_vector <- c(avg_adjusted_js_vector, 
                                function_output$avg_adjusted_js)
  }
}


avg_adjusted_js_df <- data_frame(year = year_vector,
                                 srr_name = srr_name_vector,
                                 avg_adjusted_js = avg_adjusted_js_vector)

avg_adjusted_js_df$label <- stringr::str_c("#", avg_adjusted_js_df$srr_name)
avg_adjusted_js_df$label <- factor(avg_adjusted_js_df$label,
                                   levels = stringr::str_c("#", srr_list))

# plot
ggplot(data = avg_adjusted_js_df,
       mapping = aes(x = year, y = avg_adjusted_js, group = label)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(label), ncol = 27)

  
#####################################################
## Create Average Adjusted JS for Salt Controversy ##
#####################################################

# create a dataframe to store output --
year_list <- sort(unique(V(G_salt)[V(G_salt)$type == "Systematic Review Report"]$year))
srr_list <- V(G_salt)[V(G_salt)$type == "Systematic Review Report"]$name

# a data structure to store data
year_vector <- c()
srr_name_vector <- c()
avg_adjusted_js_vector <- c()

for (year in year_list){
  for (srr_name in srr_list){
    
    function_output <- avg_adjusted_js(year=year,
                                       srr_name = srr_name,
                                       G = G_salt)
    
    year_vector <- c(year_vector, function_output$year)
    
    srr_name_vector <- c(srr_name_vector, function_output$srr_name)
    
    avg_adjusted_js_vector <- c(avg_adjusted_js_vector, 
                                function_output$avg_adjusted_js)
  }
}


avg_adjusted_js_df <- data_frame(year = year_vector,
                                 srr_name = srr_name_vector,
                                 avg_adjusted_js = avg_adjusted_js_vector)

avg_adjusted_js_df$label <- stringr::str_c("#", avg_adjusted_js_df$srr_name)
avg_adjusted_js_df$label <- factor(avg_adjusted_js_df$label,
                                   levels = stringr::str_c("#", srr_list))

# plot
ggplot(data = avg_adjusted_js_df,
       mapping = aes(x = year, y = avg_adjusted_js, group = label)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(label), ncol = 14)

