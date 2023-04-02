# functions.R
# Author: Yuanxi Fu
# This file holds necessary functions for the set of R scripts 
# in the current repository

library(igraph)
library(tidyverse)
library(reshape)
library(stringr)


# Create the overall ExRx inclusion Graph
# Three attributes: name, type, year
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

# create the overall salt controversy inclusion network
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
                          'year' = raw_attr_list$year,
                          'study_design' = raw_attr_list$study_design)
  
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
  
  # subgraph ids -- PSR
  # !! <= subgraph_year
  vids <- which(V(G)$year <= subgraph_year & 
                  V(G)$type == "Primary Study Report")
  
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
  
  # select the diagnoal element to return
  return(adjusted_js[1,2])
  
}


#
# compute adjusted jaccard similarity year minus one
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
  # !! < subgraph_year
  vids <- which(V(G)$year < subgraph_year & 
                  V(G)$type == "Primary Study Report")
  
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
  
  # select the diagnoal element to return
  return(adjusted_js[1,2])
  
}

#
# compute the dandelion-ness ratio
#
# input: the edge list, first column SRR, second column psr
# output: the SSRs and their dandelion-ness
# easier to code using the edge list rather than the graph
# !!Assume the first column is SRR and the second column is PSR

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

# compute jaccard simliarities
#
# input: an inclusion network
# output: return a dataframe with Jaccard simliarities of all SRR pairs
#

compute_js_df <- function(G){
  
  jaccard_similarities <- igraph::similarity(
    graph = G,
    vids = which(V(G)$type == "Systematic Review Report"),
    mode = 'out',
    method = 'jaccard'
  )
  
  # reshape the matrix output to a data frame
  js_df <- reshape::melt(jaccard_similarities)
  colnames(js_df) <- c("v1", "v2", "jaccard")
  
  # remove entries where v1 == v2
  rows_to_keep = which(js_df$v1 < js_df$v2)
  js_df = js_df[rows_to_keep,]
  
  # create name dict to convert idx to our IDs
  for (i in 1:nrow(js_df))
  {
    v1 <- js_df$v1[i]
    v2 <- js_df$v2[i]
    js_df$v1[i] <- as.integer(igraph::V(G)[v1]$name)
    js_df$v2[i] <- as.integer(igraph::V(G)[v2]$name)
    
  }
  
  return(js_df)
  
}