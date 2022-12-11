# d_ratio.R

# This script produces Figure 2 and Figure 5 of the manuscript "Exploring Evidence Selection with the Inclusion Network"
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

# obtain edge list
raw_edge_list_file_exrx <- "data/ExRx/inclusion_net_edges.csv"
raw_edge_list_exrx <- read_csv(raw_edge_list_file_exrx)
edge_list_exrx <- select(raw_edge_list_exrx, citing_id, cited_id)

# set input files: Salt Controversy
raw_edge_list_file_salt <- "data/salt/inclusion_net_edges.csv"
raw_edge_list_salt <- read_csv(raw_edge_list_file_salt)
edge_list_salt <- raw_edge_list_salt

# compute d_ratio ExRx
d_ratio_df_exrx <- compute_d_ratio(edge_list_exrx)

# compute d_ratio salt
d_ratio_df_salt <- compute_d_ratio(edge_list_salt)

# Figure 5
d_ratio_df_exrx$dataset <- rep("ExRx", nrow(d_ratio_df_exrx))
d_ratio_df_salt$dataset <- rep("Salt", nrow(d_ratio_df_salt))

combdat <- rbind(d_ratio_df_exrx, d_ratio_df_salt)

ggplot(data = combdat, aes(x = d_ratio)) +
  geom_histogram(binwidth = 0.05, fill = 'grey', color = 'black') +
  facet_wrap(~dataset) +
  scale_x_continuous(name = "The Dandelion-ness Ratio") +
  theme_bw() +
  theme(strip.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0, unit = 'pt'),
                                    size = 18),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0, unit = 'pt'),
                                    size = 18))

mean(d_ratio_df_exrx$d_ratio) # 0.3974397
mean(d_ratio_df_salt$d_ratio) # 0.07208308

median(d_ratio_df_exrx$d_ratio) # 0.4
median(d_ratio_df_salt$d_ratio) # 0

sqrt(var(d_ratio_df_exrx$d_ratio)) # 0.3031006
sqrt(var(d_ratio_df_salt$d_ratio)) # 0.1031187

# Figure 2
# regenerate d_ratio_df_exrx
d_ratio_df_exrx <- compute_d_ratio(edge_list_exrx)

d_ratio_df_exrx$name <- stringr::str_c("#", 
                                       as.character(d_ratio_df_exrx$srr))

isolated_srr <- c(3, 23, 26)
periphery_srr <- c(1, 4, 5, 6, 11, 13, 14, 15, 17, 18, 19, 20, 24, 27, 28)
core_srr <- c(2, 8, 9, 10, 12, 16, 21, 22, 25)

d_ratio_df_exrx$type <- case_when(
  
  d_ratio_df_exrx$srr %in% isolated_srr ~ 'isolated',
  d_ratio_df_exrx$srr %in% periphery_srr ~ 'periphery',
  d_ratio_df_exrx$srr %in% core_srr ~ 'core'
)

d_ratio_df_exrx$type <- factor(d_ratio_df_exrx$type, levels = c('core', 'periphery', 'isolated'))

# create a color-blind friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73")

ggplot(data=d_ratio_df_exrx, aes(x=reorder(name, -d_ratio), y=d_ratio, fill=type, color=type)) +
  geom_bar(stat = "identity") +
  labs(y = "Dandelion-ness Ratio", x = 'SRR') +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme_bw() +
  theme(legend.title= element_blank(), 
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0, unit = 'pt'),
                                    size = 18),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0, unit = 'pt'),
                                    size = 18))


