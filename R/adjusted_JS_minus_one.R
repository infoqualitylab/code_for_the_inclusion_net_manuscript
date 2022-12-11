#
# adjusted_JS_minus.R
#
# This file generate Figure S1 and Figure S2 of manuscript "Exploring Evidence Selection with the Inclusion Network"
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

#####################################################
## Create Average Adjusted JS time series for ExRx ##
#####################################################

# create a dataframe to store output --
year_list <- sort(unique(V(G_exrx)[V(G_exrx)$type == "Systematic Review Report"]$year))
srr_list <- V(G_exrx)[V(G_exrx)$type == "Systematic Review Report"]$name

# a data structure to store data
srr_1_vector <- c()
srr_2_vector <- c()
adjusted_js_vector <- c()

for (srr_1_name in srr_list){
  
  for (srr_2_name in srr_list) {
    
    if (srr_1_name != srr_2_name){
      
      srr_1_vector <- c(srr_1_vector, srr_1_name)
      srr_2_vector <- c(srr_2_vector, srr_2_name)
      
      adjusted_js_vector <- c(adjusted_js_vector, 
                              adjusted_js_minus_one(srr_1_name, srr_2_name, G_exrx))
      
      
    }
  }
}

adjusted_js_df <- data.frame("srr_1_name" = srr_1_vector, 
                             "srr_2_name" = srr_2_vector, 
                             "adjusted_js" = adjusted_js_vector)

# clean off redundant rows: (v1, v2) and (v2, v1)
# use a simple math since all nodes are also numbers
# may not work with text node names
rows_to_keep <- which(as.integer(adjusted_js_df$srr_1_name) 
                      < as.integer(adjusted_js_df$srr_2_name))

adjusted_js_df <- adjusted_js_df[rows_to_keep,]

search_date_df <- read.csv(raw_search_date_file_exrx)

# append search year
# first convert datatype to character for merge
search_date_df$article_id <- as.character(search_date_df$article_id)
search_date_df <- select(search_date_df, "article_id", "search_year")

# merge data frames to add year
adjusted_js_df_with_year <- inner_join(x=adjusted_js_df, 
                                       y=search_date_df, 
                                       by=c('srr_1_name' = 'article_id'))

colnames(adjusted_js_df_with_year) <- c("srr_1_name", "srr_2_name", "adjusted_js", "srr_1_year")

adjusted_js_df_with_year <- inner_join(x=adjusted_js_df_with_year, 
                                       y=search_date_df, 
                                       by=c('srr_2_name' = 'article_id'))

colnames(adjusted_js_df_with_year) <- c("srr_1_name", "srr_2_name", "adjusted_js", 
                                        "srr_1_year", "srr_2_year")

# prepare vectors
year_vector <- c()
srr_name_vector <- c()
avg_adjusted_js_vector <- c()

# looping through srr and year and computer average
for (srr in srr_list){
  
  for (year in year_list){
    
    temp_df_1 <- adjusted_js_df_with_year %>% filter(srr_1_name == srr)
    temp_df_2 <- adjusted_js_df_with_year %>% filter(srr_2_name == srr)
    
    #swap srr_1 and srr_2 if srr_2 == srr
    
    if (nrow(temp_df_2)){
      
      for (i in 1:nrow(temp_df_2)){
        
        temp_srr <- temp_df_2$srr_2_name[i]
        temp_df_2$srr_2_name[i] <- temp_df_2$srr_1_name[i]
        temp_df_2$srr_1_name[i] <- temp_srr
        
        temp_srr_year <- temp_df_2$srr_2_year[i]
        temp_df_2$srr_2_year[i] <- temp_df_2$srr_1_year[i]
        temp_df_2$srr_1_year[i] <- temp_srr_year
      }
    }
    
    # use temp_df to computer average adjusted js for 
    temp_df <- rbind(temp_df_1, temp_df_2)
    
    temp_df <- filter(temp_df, srr_2_year <= year)
    
    avg_adjusted_js_vector <- c(avg_adjusted_js_vector, mean(temp_df$adjusted_js))
    year_vector <- c(year_vector, year)
    srr_name_vector <- c(srr_name_vector, srr)
  }
  
}

# data structure of the resulted dataframe
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
  labs(y = "Average Adjusted Jaccard Similarity\nYear Minus-one", x = 'Year') +
  theme(strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0, unit = 'pt'),
                                    size = 18),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0, unit = 'pt'),
                                    size = 18))

#####################################################
## Create Average Adjusted JS for Salt Controversy ##
#####################################################

year_list <- sort(unique(V(G_salt)[V(G_salt)$type == "Systematic Review Report"]$year))
srr_list <- V(G_salt)[V(G_salt)$type == "Systematic Review Report"]$name

# a data structure to store data
srr_1_vector <- c()
srr_2_vector <- c()
adjusted_js_vector <- c()

for (srr_1_name in srr_list){
  
  for (srr_2_name in srr_list) {
    
    if (srr_1_name != srr_2_name){
      
      srr_1_vector <- c(srr_1_vector, srr_1_name)
      srr_2_vector <- c(srr_2_vector, srr_2_name)
      
      adjusted_js_vector <- c(adjusted_js_vector, 
                              adjusted_js_minus_one(srr_1_name, srr_2_name, G_salt))
    }
  }
}

adjusted_js_df <- data.frame("srr_1_name" = srr_1_vector, 
                             "srr_2_name" = srr_2_vector, 
                             "adjusted_js" = adjusted_js_vector)

# clean off redundant rows: (v1, v2) and (v2, v1)
# use a simple math since all nodes are also numbers
# may not work with text node names
rows_to_keep <- which(as.integer(adjusted_js_df$srr_1_name) 
                      < as.integer(adjusted_js_df$srr_2_name))

adjusted_js_df <- adjusted_js_df[rows_to_keep,]

search_date_df <- read.csv(raw_search_date_file_salt)

# append search year
# first convert datatype to character for merge
search_date_df$ID <- as.character(search_date_df$ID)
search_date_df <- select(search_date_df, "ID", "last_search_year")

# merge data frames to add year
adjusted_js_df_with_year <- inner_join(x=adjusted_js_df, 
                                       y=search_date_df, 
                                       by=c('srr_1_name' = 'ID'))

colnames(adjusted_js_df_with_year) <- c("srr_1_name", "srr_2_name", "adjusted_js", "srr_1_year")

adjusted_js_df_with_year <- inner_join(x=adjusted_js_df_with_year, 
                                       y=search_date_df, 
                                       by=c('srr_2_name' = 'ID'))

colnames(adjusted_js_df_with_year) <- c("srr_1_name", "srr_2_name", "adjusted_js", 
                                        "srr_1_year", "srr_2_year")

# prepare vectors
year_vector <- c()
srr_name_vector <- c()
avg_adjusted_js_vector <- c()

# looping through srr and year and computer average
for (srr in srr_list){
  
  for (year in year_list){
    
    temp_df_1 <- adjusted_js_df_with_year %>% filter(srr_1_name == srr)
    temp_df_2 <- adjusted_js_df_with_year %>% filter(srr_2_name == srr)
    
    #swap srr_1 and srr_2 if srr_2 == srr
    
    if (nrow(temp_df_2)){
      
      for (i in 1:nrow(temp_df_2)){
        
        temp_srr <- temp_df_2$srr_2_name[i]
        temp_df_2$srr_2_name[i] <- temp_df_2$srr_1_name[i]
        temp_df_2$srr_1_name[i] <- temp_srr
        
        temp_srr_year <- temp_df_2$srr_2_year[i]
        temp_df_2$srr_2_year[i] <- temp_df_2$srr_1_year[i]
        temp_df_2$srr_1_year[i] <- temp_srr_year
      }
    }
    
    # use temp_df to computer average adjusted js for 
    temp_df <- rbind(temp_df_1, temp_df_2)
    
    temp_df <- filter(temp_df, srr_2_year <= year)
    
    avg_adjusted_js_vector <- c(avg_adjusted_js_vector, mean(temp_df$adjusted_js))
    year_vector <- c(year_vector, year)
    srr_name_vector <- c(srr_name_vector, srr)
  }
  
}

# data structure of the resulted dataframe
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
      V(G_salt)[which(V(G_salt)$name == avg_adjusted_js_df$srr_name[i])]$year){
    
    avg_adjusted_js_df$is_search_year[i] = 1
    
  }
}

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
  labs(y = "Average Adjusted Jaccard Similarity\nYear Minus-one", x = 'Year') + 
  theme(strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0, unit = 'pt'),
                                    size = 18),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0, unit = 'pt'),
                                    size = 18))


