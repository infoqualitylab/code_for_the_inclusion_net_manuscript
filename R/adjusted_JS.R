
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

avg_adjusted_js_df$is_search_year <- rep(0, nrow(avg_adjusted_js_df))

# add color column
for (i in 1:nrow(avg_adjusted_js_df)){
  
  if (avg_adjusted_js_df$year[i] == 
      V(G_exrx)[which(V(G_exrx)$name == avg_adjusted_js_df$srr_name[i])]$year){
    
    avg_adjusted_js_df$is_search_year[i] = 1
    
  }
}

# plot
# # one row
# ggplot(data = avg_adjusted_js_df,
#        mapping = aes(x = year, y = avg_adjusted_js, group = label, color = color)) +
#   geom_line(color = "black", show.legend = FALSE) +
#   geom_point(show.legend = FALSE) +
#   facet_wrap(vars(label), ncol = 27) + 
#   scale_x_continuous(limits = c(2010, 2020), breaks = c(2012, 2018), labels = c(12, 18)) +
#   scale_y_continuous(limits = c(0, 0.20)) +
#   labs(y = "Average Adjusted Jaccard Similarity", x = 'Year')
#   
# # two rows
# ggplot(data = avg_adjusted_js_df,
#        mapping = aes(x = year, y = avg_adjusted_js, group = label, color = color)) +
#   geom_line(color = "black", show.legend = FALSE) +
#   geom_point(show.legend = FALSE) +
#   facet_wrap(vars(label), ncol = 14, nrow = 2) + 
#   scale_x_continuous(limits = c(2010, 2020), breaks = c(2012, 2018), labels = c(2012, 2018)) +
#   scale_y_continuous(limits = c(0, 0.20)) +
#   labs(y = "Average Adjusted Jaccard Similarity", x = 'Year')

# change color
my_colors = c("black", "red")
ggplot(data = avg_adjusted_js_df,
       mapping = aes(x = year, y = avg_adjusted_js, group = label, color = factor(is_search_year))) +
  geom_line(color = "black", show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(vars(label), ncol = 27) + 
  scale_x_continuous(limits = c(2010, 2020), breaks = c(2012, 2018), labels = c(12, 18)) +
  scale_y_continuous(limits = c(0, 0.20)) +
  scale_color_manual(values = my_colors) +
  labs(y = "Average Adjusted Jaccard Similarity", x = 'Year')

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

# loop over year and srr to fill out the vectors
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

# create a data frame
avg_adjusted_js_df <- data_frame(year = year_vector,
                                 srr_name = srr_name_vector,
                                 avg_adjusted_js = avg_adjusted_js_vector)

avg_adjusted_js_df$label <- stringr::str_c("#", avg_adjusted_js_df$srr_name)
avg_adjusted_js_df$label <- factor(avg_adjusted_js_df$label,
                                   levels = stringr::str_c("#", srr_list))

# zero: the year is not the search year for the target SRR
avg_adjusted_js_df$is_search_year <- rep(0, nrow(avg_adjusted_js_df))

# add color column
for (i in 1:nrow(avg_adjusted_js_df)){
  
  if (avg_adjusted_js_df$year[i] == 
      V(G_salt)[which(V(G_salt)$name == avg_adjusted_js_df$srr_name[i])]$year){
    # 1: the year is the search year for the target SRR
    avg_adjusted_js_df$is_search_year[i] = 1
  
    }
}

# avg_adjusted_js_df$is_search_year <- factor(avg_adjusted_js_df$is_search_year)

# plot
my_colors <- c("black", "red" )
ggplot(data = avg_adjusted_js_df, 
       mapping = aes(x = year, y = avg_adjusted_js, group = label, color = factor(is_search_year))) +
  geom_point(show.legend = FALSE) +
  geom_line(color="black", show.legend = FALSE) +
  facet_wrap(vars(label), ncol = 14) +
  scale_x_continuous(limits = c(2000,2015), breaks = c(2002, 2008, 2014), labels = c("02", "08", "14")) +
  scale_y_continuous(limits = c(0, 1.0)) +
  scale_color_manual(values = my_colors) +
  labs(y = "Average Adjusted Jaccard Similarity", x = 'Year')
