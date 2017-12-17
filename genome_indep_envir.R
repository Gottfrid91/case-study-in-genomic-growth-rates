#case study I
#load packages
library(data.table)
library(tidyverse)
library(magrittr)
require(gridExtra)
require(grid)

#set data dir
DATA_DIR <- c('')

marker_file <- file.path(DATA_DIR, 'marker.txt')
growth_file <- file.path(DATA_DIR, 'growth.txt')
genotype_file <- file.path(DATA_DIR, 'genotype.txt')

#read in data
marker <- read.delim(marker_file)
growth <- read.delim(growth_file)
genotype <- read.delim(genotype_file)

mygeno <- genotype[, which(marker$chrom=="chr07" & marker$start== 1069229)]
mygeno1 <- genotype[, which(marker$chrom=="chr01" & marker$start== 1512)+1]

names(mygeno) <- genotype$strain
names(mygeno1) <- genotype$strain

#convert growth to a data table
growth_dt <- as.data.table(growth)

#style specifications
pbox <- geom_boxplot() 
x_lab <- xlab("") 
outlook <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

##create plot for grid
p1 <- ggplot(data = growth , aes(x=mygeno, y=YPD)) + pbox  + x_lab +outlook
p2 <- ggplot(data = growth , aes(x=mygeno, y=YPD_BPS)) + pbox  + x_lab +outlook
p3 <- ggplot(data = growth , aes(x=mygeno, y=YPD_Rapa)) + pbox + x_lab +outlook
p4 <- ggplot(data = growth , aes(x=mygeno, y=YPE)) + pbox + x_lab +outlook
p5 <- ggplot(data = growth , aes(x=mygeno, y=YPMalt)) + pbox + x_lab +outlook

#assemble plot on grid
grid.arrange(p1,p2,p3,p4,p5, nrow=2, top=textGrob("Does genotype affect fitness indepedent of environment", gp=gpar(fontsize=15,font=8)))
