
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
  edge_list <- data_frame('from' = raw_edge_list$citing_id,
                          'to' = raw_edge_list$cited_id)
  
  
  attr_list <- data_frame('id' = raw_attr_list$article_id,
                          'type' = raw_attr_list$node_type,
                          'year' = raw_attr_list$publication_year)
  
  search_date <- data_frame('id' = raw_search_date$article_id,
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
  edge_list <- data_frame('from' = raw_edge_list$citing_ID,
                          'to' = raw_edge_list$cited_ID)
  
  
  attr_list <- data_frame('id' = raw_attr_list$ID,
                          'type' = raw_attr_list$node_type,
                          'year' = raw_attr_list$year)
  
  search_date <- data_frame('id' = raw_search_date$ID,
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

# return adjusted average jaccard similarity for an SRR in a specific year
avg_adjusted_js <- function(year,
                            srr_name,
                            G){
  
  # create vids
  vids <- which(V(G)$year <= year)
  
  if (!which(V(G)$name == srr_name) %in% vids){
    
    vids <- c(vids, which(V(G)$name == srr_name))
              
  }
  
  # construct the subgraph
  G_sub <- igraph::induced.subgraph(graph=G, vids = vids)
  
  # compute Jaccard Similarity
  srr_vids <- which(V(G_sub)$type == "Systematic Review Report")
  
  js_matrix <- igraph::similarity(
    graph = G_sub,
    vids = srr_vids,
    mode = 'out',
    method = 'jaccard'
  )
  
  # compute average by: melt the js_matrix, remove repeats and diagonals, compute average
  js_df <- reshape::melt(js_matrix)
  colnames(js_df) <- c("v1", "v2", "jaccard")
  
  # remove entries where v1 == v2
  rows_to_keep = which(js_df$v1 < js_df$v2)
  # fix
  
  js_df = js_df[rows_to_keep,]
  avg_adjusted_js <- mean(js_df$jaccard)
  
  return(list(year=year, 
              srr_name = srr_name, 
              avg_adjusted_js = avg_adjusted_js))
}