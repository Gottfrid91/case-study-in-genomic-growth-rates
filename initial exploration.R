#genomics tutorial for ideas: http://varianceexplained.org/r/tidy-genomics/

#case study I
#load packages
library(data.table)
library(tidyverse)
library(magrittr)

#set data dir
DATA_DIR <- c('/home/olle/Documents/University Courses/Data analysis and vizulisation/Data visualization in R/exercises&lectures/data')

marker_file <- file.path(DATA_DIR, 'marker.txt')
growth_file <- file.path(DATA_DIR, 'growth.txt')
genotype_file <- file.path(DATA_DIR, 'genotype.txt')

#read in data
marker <- as.data.table(read.delim(marker_file))
growth <- as.data.table(read.delim(growth_file))
genotype <- as.data.table(read.delim(genotype_file))

#first look at data properties - genotype
head(genotype,1)
dim(genotype)
colnames(genotype)

#transforming to long format
genotype_long <- melt(data = genotype, id.vars = "strain")
#rename columns
setnames(genotype_long, c("variable", "value"), c("Genotype", "value"))
#check dim
dim(genotype_long)

#plot most commom genotype value in yeast
ggplot(data=genotype_long, aes(value)) + stat_count()

#ideas: does the distribution of genotype "value" differ among the strains? What environment creates the best growth on avarage

#first look at data properties - growth
head(growth)
dim(growth)
colnames(growth)

#creating a column with avarge growth
growth[,avarage_growth := sum(YPD,YPD_BPS,YPD_Rapa,YPE,YPMalt, na.rm = TRUE)/length(growth), by = strain]

#check columns
head(growth)

#plot avarage growth in all environments for all strains
ggplot(data = growth, aes(avarage_growth)) + geom_dotplot()

#idea: investigate the extrem points, see if certain strains are specielizing in certain environmens

#first look at data properties - marker
head(marker)
dim(marker)
colnames(marker)

#idea: add length of marker and see if it correlates with any growth rate informmation
