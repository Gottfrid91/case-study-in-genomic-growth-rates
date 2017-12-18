#case study I
#load packages
library(data.table)
library(tidyverse)
library(magrittr)
require(gridExtra)
require(grid)

#set data dir
DATA_DIR <- c('')

gene_file <- file.path(DATA_DIR, 'gene.txt')
expression_file <- file.path(DATA_DIR, 'expression.txt')

#read in data
gene <- read.delim(gene_file)
expression <- fread(expression_file)

#select columns
indx <- grepl('seg_45C', colnames(expression))

#data for plot
heat_map <- expression[which(gene$chrom=="chr01")][,indx, with = FALSE]
heat_map[,rown := rownames(heat_map)]
heat_map_long <- melt(heat_map, id.vars = "rown")

outlook <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(heat_map_long, aes(x=rown , y=variable)) +
  geom_tile(aes(fill = value), color = "white") + outlook  +ylab("") + xlab("Genes") + 
  ggtitle("Cromosone 1: Segregant 47C")
