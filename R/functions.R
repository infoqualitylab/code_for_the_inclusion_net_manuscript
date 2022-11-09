# functions.R
# author: Yuanxi Fu
# this file holds necessary functions for the set of R script 
# in the current repository

library(igraph)
library(tidyverse)
library(reshape)
library(stringr)


# Create the overall ExRx inclusion Graph
# Three attributes: Name, Type, Year
make_exrx_graph <- function(raw_edge_list_file, 
                                    raw_attr_list_file, 
                                    raw_search_date_file)
{
  
  raw_edge_list <- read.csv(raw_edge_list_file)
  raw_attr_list <- read.csv(raw_attr_list_file)
  raw_search_date <- read.csv(raw_search_date_file)
  
  # filter the raw data
  edge_list <- data.frame('from' = raw_edge_list$citing_id,
                          'to' = raw_edge_list$cited_id)
  
  
  attr_list <- data.frame('id' = raw_attr_list$article_id,
                          'type' = raw_attr_list$node_type,
                          'year' = raw_attr_list$publication_year)
  
  search_date <- data.frame('id' = raw_search_date$article_id,
                            'search_year' = raw_search_date$search_year,
                            'short_name' = raw_search_date$short_name)
  
  # replace year field of reviews (publication year) 
  # with year field in the raw_search_date (last search year)
  for (i in search_date$id) {
    
    attr_list$year[which(attr_list$id == i)] <-
      search_date$search_year[which(search_date$id == i)]
    
  }
  
  # create graph
  g <- igraph::graph_from_data_frame(
    d = edge_list,
    vertices = attr_list
  )
}

# create the overall Salt Controversy Inclusion Network
# Three attribute: name, type, year

create_salt_graph <- function(raw_edge_list_file, 
                                   raw_attr_list_file, 
                                   raw_search_date_file)
  {
  raw_edge_list <- read.csv(raw_edge_list_file)
  raw_attr_list <- read.csv(raw_attr_list_file)
  raw_search_date <- read.csv(raw_search_date_file)
  
  # filter the raw data
  edge_list <- data.frame('from' = raw_edge_list$citing_ID,
                          'to' = raw_edge_list$cited_ID)
  
  
  attr_list <- data.frame('id' = raw_attr_list$ID,
                          'type' = raw_attr_list$node_type,
                          'year' = raw_attr_list$year)
  
  search_date <- data.frame('id' = raw_search_date$ID,
                            'search_year' = raw_search_date$last_search_year, 
                            'short_name' = raw_search_date$short_name)
  
  
  # replace year field of reviews with year field in the raw_search_date
  for (i in search_date$id) {
    
    attr_list$year[which(attr_list$id == i)] <- 
      search_date$search_year[which(search_date$id == i)] 
    
  }
  
  # create graph
  g <- igraph::graph_from_data_frame(
    d = edge_list,
    vertices = attr_list
  )
}

#
# compute adjusted jaccard similarity
#
# input: the two srrs' name
# output: their adjusted Jaccard similarity
adjusted_js <- function(srr_1_name, srr_2_name, G){
  
  # convert name to id
  srr_1_id <- which(V(G)$name == srr_1_name)
  srr_2_id <- which(V(G)$name == srr_2_name)
  
  # find the search year of srr_1 and srr_2
  srr_1_year <- V(G)[srr_1_id]$year
  srr_2_year <- V(G)[srr_2_id]$year
  
  # chose smaller of srr_1_year and srr_2_year as the subgraphing year
  subgraph_year <- min(srr_1_year, srr_2_year)
  
  # subgraph ids
  vids <- which(V(G)$year <= subgraph_year)
  
  # add back srr_1_id or srr_2_id if either of them is not in the vids
  if (!(srr_1_id %in% vids)){
    
    vids <- c(vids, srr_1_id)
    
  }
  
  if (!(srr_2_id %in% vids)){
    
    vids <- c(vids, srr_2_id)
    
  }  
  
  
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
  
  # select the diagnoal element to return
  return(adjusted_js[1,2])
  
}


#
# compute adjusted jaccard similarity
#
# input: the two srrs' name
# output: their adjusted Jaccard similarity
adjusted_js_minus_one <- function(srr_1_name, srr_2_name, G){
  
  # convert name to id
  srr_1_id <- which(V(G)$name == srr_1_name)
  srr_2_id <- which(V(G)$name == srr_2_name)
  
  # find the search year of srr_1 and srr_2
  srr_1_year <- V(G)[srr_1_id]$year
  srr_2_year <- V(G)[srr_2_id]$year
  
  # chose smaller of srr_1_year and srr_2_year as the subgraphing year
  subgraph_year <- min(srr_1_year, srr_2_year)
  
  # subgraph ids
  vids <- which(V(G)$year < subgraph_year)
  
  # add back srr_1_id or srr_2_id if either of them is not in the vids
  if (!(srr_1_id %in% vids)){
    
    vids <- c(vids, srr_1_id)
    
  }
  
  if (!(srr_2_id %in% vids)){
    
    vids <- c(vids, srr_2_id)
    
  }  
  
  
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
  
  # select the diagnoal element to return
  return(adjusted_js[1,2])
  
}