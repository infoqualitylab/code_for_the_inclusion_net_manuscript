# R1_functions.R
# Author: Yuanxi Fu
# Description: This file holds necessary functions for the set of R scripts 
# in the current folder: 
# https://github.com/infoqualitylab/code_for_the_inclusion_net_manuscript/tree/main/R1

library(igraph)
library(tidyverse)
library(reshape)
library(stringr)
library(data.table)
library(stringr)
library(scales)
library(lubridate)

# seperated edge_list and attribute list making functions for the two datasets
# due to slight variations between the two datasets
make_edge_list_exrx <- function(exrx_edge_list_file_path){
  
  raw_edge_list <- read.csv(exrx_edge_list_file_path)
  edge_list_exrx <- data.frame('from' = raw_edge_list$citing_id,
                          'to' = raw_edge_list$cited_id)
  
  return(edge_list_exrx)
  
}


make_attr_list_exrx <- function(exrx_report_list_file_path, 
                                exrx_srr_search_date_file_path){
  
  
  # load input files
  report_list <- read.csv(exrx_report_list_file_path)
  srr_search_date <- read.csv(exrx_srr_search_date_file_path)
  srr_search_date <- srr_search_date  %>% dplyr::select(article_id, search_year, search_month, search_day)
  
  # concatenate the search_year, search_month, and search_day
  # when search_day is N/A, concatenate as Y-m
  # when search_day is not N/A, concatenate as m/d/Y
  
  srr_search_date_vector <- c()
  
  for (i in (1:nrow(srr_search_date))){
    
    if (srr_search_date[i, ]$search_day == 'N/A'){
      
      srr_search_date_vector <- c(srr_search_date_vector, 
                                  stringr::str_c(srr_search_date[i, ]$search_year,
                                                 '-',
                                                 srr_search_date[i, ]$search_month
                                  ))
    }else{
      
      srr_search_date_vector <- c(srr_search_date_vector, 
                                  stringr::str_c(srr_search_date[i, ]$search_month,
                                                 '/',
                                                 srr_search_date[i, ]$search_day,
                                                 '/',
                                                 srr_search_date[i, ]$search_year
                                  ))
      
      
    }
    
  }
  
  # create a new column called "temporal_seq_date" to store the date used 
  # to compute temporal seq
  srr_search_date$temporal_seq_date <- srr_search_date_vector
  srr_search_date <- dplyr::select(srr_search_date, article_id, temporal_seq_date)
  
  # merge srr_search_date and report_list
  report_list <- dplyr::left_join(x=report_list, y=srr_search_date, by='article_id')
  
  # fill the temporal_seq_date field with pub_date 
  # if the temporal_seq_date field is NA
  for (i in (1:nrow(report_list))){
    
    if (is.na(report_list[i,]$temporal_seq_date)){
      
      report_list[i,]$temporal_seq_date <- report_list[i,]$date
      
      
    }
    
  }
  
  date_vector <- report_list$temporal_seq_date
  
  # resolve date format difference in temporal_seq_date column
  # convert format Y to 7/1/Y (mid of the year)
  pattern <- "(^[0-9]{4}$)"
  replacement <- "7/1/\\1"
  date_vector_1 <- stringr::str_replace_all(date_vector, 
                                            pattern = pattern,
                                            replacement = replacement)
  
  # convert format Y-m to m/15/Y (mid of the month)
  pattern <- "(^[0-9]{4})-([0-9]{1,2}$)"
  replacement <- "\\2/15/\\1"
  date_vector_2 <- stringr::str_replace_all(date_vector_1, 
                                            pattern = pattern,
                                            replacement = replacement)
  
  # parse string into date data format
  date_vector_parsed <- lubridate::parse_date_time(date_vector_2, orders = c("m/d/Y"))
  report_list$temporal_seq_date_same_precision <- date_vector_2
  report_list$temporal_seq_date_parsed <- date_vector_parsed
  # create a rank vector
  rank_vector <- rank(date_vector_parsed)
  
  # add a new column to report_list data frame called "temporal_seq_rank 
  # to store the rank vector
  report_list$temporal_seq_rank <- rank_vector
  
  return(report_list)

}

make_edge_list_salt <- function(salt_edge_list_file_path){
  
  raw_edge_list <- read.csv(salt_edge_list_file_path)
  edge_list_salt <- data.frame('from' = raw_edge_list$citing_ID,
                          'to' = raw_edge_list$cited_ID)
  return(edge_list_salt)
  
}


# mainly, we add a temporal_seq_rank column to the report_list
# returned dataframe is the attr_list for graph creation
make_attr_list_salt <- function(salt_report_list_file_path, 
                                salt_srr_search_date_file_path)
  {
  
  # load input files
  report_list <- read.csv(salt_report_list_file_path)
  srr_search_date <- read.csv(salt_srr_search_date_file_path)
  srr_search_date <- srr_search_date  %>% dplyr::select(ID, last_search_year, last_search_month, last_search_day)
  
  # concatnate the last_search_year, last_search_month, and last_search_day
  # when last_search_day is NA, concatenate as Y-m
  # when last_search_day is not NA, concatenate as m/d/Y
  
  srr_search_date_vector <- c()
  
  for (i in (1:nrow(srr_search_date))){
    
    if (is.na(srr_search_date[i, ]$last_search_day)){
      
      srr_search_date_vector <- c(srr_search_date_vector, 
                                  stringr::str_c(srr_search_date[i, ]$last_search_year,
                                                 '-',
                                                 srr_search_date[i, ]$last_search_month
                                  ))
      
    }else{
      
      srr_search_date_vector <- c(srr_search_date_vector, 
                                  stringr::str_c(srr_search_date[i, ]$last_search_month,
                                                 '/',
                                                 srr_search_date[i, ]$last_search_day,
                                                 '/',
                                                 srr_search_date[i, ]$last_search_year
                                  ))
      
      
    }
    
  }
  
  # create a new column called "temporal_seq_date" to store the date used 
  # to compute temporal seq
  srr_search_date$temporal_seq_date <- srr_search_date_vector
  srr_search_date <- dplyr::select(srr_search_date, ID, temporal_seq_date)
  
  # merge srr_search_date and report_list
  report_list <- dplyr::left_join(x=report_list, y=srr_search_date, by='ID')
  
  # fill the temporal_seq_date field with pub_date 
  # if the temporal_seq_date field is NA
  for (i in (1:nrow(report_list))){
    
    if (is.na(report_list[i,]$temporal_seq_date)){
      
      report_list[i,]$temporal_seq_date <- report_list[i,]$pub_date
      
      
    }
    
  }
  
  date_vector <- report_list$temporal_seq_date
  
  # resolve date format difference in temporal_seq_date column
  # convert format Y to 7/1/Y (mid of the year)
  pattern <- "(^[0-9]{4}$)"
  replacement <- "7/1/\\1"
  date_vector_1 <- stringr::str_replace_all(date_vector, 
                                            pattern = pattern,
                                            replacement = replacement)
  
  # convert format Y-m to m/15/Y (mid of the month)
  pattern <- "(^[0-9]{4})-([0-9]{1,2}$)"
  replacement <- "\\2/15/\\1"
  date_vector_2 <- stringr::str_replace_all(date_vector_1, 
                                            pattern = pattern,
                                            replacement = replacement)
  
  # parse string into date data format
  date_vector_parsed <- lubridate::parse_date_time(date_vector_2, orders = c("m/d/Y"))
  report_list$temporal_seq_date_same_precision <- date_vector_2
  report_list$temporal_seq_date_parsed <- date_vector_parsed
  
  # create a rank vector
  rank_vector <- rank(date_vector_parsed)
  
  # add a new column to report_list data frame called "temporal_seq_rank 
  # to store the rank vector
  report_list$temporal_seq_rank <- rank_vector
  
  
  return(report_list)
  
}


# a general function to create graph
make_graph <- function(attr_list, edge_list){
  
  G <- igraph::graph_from_data_frame(
    d = edge_list,
    vertices = attr_list
  )
  
  return(G)
  
}

adjusted_js <- function(srr_1_name, srr_2_name, G){
  
  # convert name to id, because the id are sequential but not the IDs (i.e., name)
  srr_1_id <- which(V(G)$name == srr_1_name)
  srr_2_id <- which(V(G)$name == srr_2_name)
  
  # find the rank of srr_1 and srr_2
  srr_1_rank <- V(G)[srr_1_id]$temporal_seq_rank
  srr_2_rank <- V(G)[srr_2_id]$temporal_seq_rank
  
  # the threshold_rank is the smaller one of srr_1_rank and srr_2_rank
  threshold_rank <- min(srr_1_rank, srr_2_rank)
  
  # select the PSRs whose rank are equal or less than the threshold_rank
  vids <- which(V(G)$temporal_seq_rank <= threshold_rank & 
                  V(G)$node_type == "Primary Study Report")
  
  # affix srr_1_id and srr_2_id
  vids <- c(vids, srr_1_id, srr_2_id)
  
  # construct the subgraph
  G_sub <- igraph::induced.subgraph(graph=G, vids = vids)
  
  # recompute vids for srr_1 and srr_2
  srr_1_id_sub <- which(V(G_sub)$name == srr_1_name)
  srr_2_id_sub <- which(V(G_sub)$name == srr_2_name)
  
  # compute Jaccard similarity of srr_1 and srr_2
  
  adjusted_js <- igraph::similarity(
    
    graph = G_sub,
    vids = c(srr_1_id_sub, srr_2_id_sub),
    mode = "out",
    method = 'jaccard'
  )
  
  # select the diagonal element to return
  return(round(adjusted_js[1,2],3))
  
}

compute_adj_js_df <- function(G){

  srr_list <- V(G)[V(G)$node_type == "Systematic Review Report"]$name
  
  # a data structure to store data
  srr_1_vector <- c()
  srr_2_vector <- c()
  adjusted_js_vector <- c()
  
  for (srr_1_name in srr_list){
    
    for (srr_2_name in srr_list) {
      
      if (as.numeric(srr_1_name) < as.numeric(srr_2_name))
        
        {
        
        srr_1_vector <- c(srr_1_vector, srr_1_name)
        srr_2_vector <- c(srr_2_vector, srr_2_name)
        
        adjusted_js_vector <- c(adjusted_js_vector, 
                                adjusted_js(srr_1_name, srr_2_name, G))
        
        
      }
    }
  }
  
  adjusted_js_df <- data.frame("srr_1_name" = srr_1_vector, 
                               "srr_2_name" = srr_2_vector, 
                               "adjusted_js" = adjusted_js_vector)
  
  return(adjusted_js_df)
  
}


compute_d_ratio <- function(edge_list){
  
  # unify column names
  colnames(edge_list) <- c("from", "to")
  
  # assume the first column is srr
  srr_names <- as.vector(unlist(unique(edge_list[1])))
  
  d_ratio <- c()
  
  for (i in srr_names){
    
    
    # get the number of psrs included in an srr
    total_no_psr <- nrow(filter(edge_list, from == i))
    
    psr_names <- as.vector(unlist(filter(edge_list, from == i) %>% select(to)))
    
    total_lingering_psr <- 0
    
    # looping through psrs to find 
    for (j in psr_names) {
      
      if (nrow(filter(edge_list, to == j)) == 1){
        
        total_lingering_psr <- total_lingering_psr + 1
        
      }
    }
    
    d_ratio <- c(d_ratio, total_lingering_psr/total_no_psr)
    
  }
  
  return(tibble("srr"=srr_names, "d_ratio"=d_ratio))
  
}

# add a regular JS computation routine
reg_js <- function(srr_1_name, srr_2_name, G){
  
  # convert name to id, because the id are sequential but not the IDs (i.e., name)
  srr_1_id <- which(V(G)$name == srr_1_name)
  srr_2_id <- which(V(G)$name == srr_2_name)
  
  # compute Jaccard similarity of srr_1 and srr_2
  
  reg_js <- igraph::similarity(
    
    graph = G,
    vids = c(srr_1_id, srr_2_id),
    mode = "out",
    method = 'jaccard'
  )
  
  # select the diagonal element to return
  return(round(reg_js[1,2],3))
  
}

# add a regular JS dataframe computation routine
compute_reg_js_df <- function(G){
  
  srr_list <- V(G)[V(G)$node_type == "Systematic Review Report"]$name
  
  # a data structure to store data
  srr_1_vector <- c()
  srr_2_vector <- c()
  reg_js_vector <- c()
  
  for (srr_1_name in srr_list){
    
    for (srr_2_name in srr_list) {
      
      if (as.numeric(srr_1_name) < as.numeric(srr_2_name))
        
      {
        
        srr_1_vector <- c(srr_1_vector, srr_1_name)
        srr_2_vector <- c(srr_2_vector, srr_2_name)
        
        reg_js_vector <- c(reg_js_vector,
                           reg_js(srr_1_name, srr_2_name, G))
        
        
      }
    }
  }
  
  reg_js_df <- data.frame("srr_1_name" = srr_1_vector, 
                               "srr_2_name" = srr_2_vector, 
                               "reg_js" = reg_js_vector)
  
  return(reg_js_df)
  
}

