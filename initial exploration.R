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
unique(genotype$strain)

#transforming to long format
genotype_long <- melt(data = genotype, id.vars = "strain")

#rename columns
setnames(genotype_long, c("variable", "value"), c("Genotype", "value"))

#check dim
head(genotype_long)
dim(genotype_long)
unique(genotype_long$Genotype)

#plot most commom genotype value in yeast
ggplot(data=genotype_long, aes(value)) + stat_count()

#ideas: does the distribution of genotype "value" differ among the strains? What environment creates the best growth on avarage

#first look at data properties - growth
head(growth)
dim(growth)

#creating a column with avarge growth
growth[,avarage_growth := sum(YPD,YPD_BPS,YPD_Rapa,YPE,YPMalt, na.rm = TRUE)/length(growth), by = strain]

#make into tidy format
growth_long <- melt(data = growth, id.vars = c("strain", "avarage_growth"))
setnames(growth_long,c("variable", "value"), c("Nutrition", "growth_rate"))

#check columns
head(growth)
dim(growth)

#plot avarage growth in all environments for all strains
ggplot(data = growth, aes(avarage_growth)) + geom_dotplot()

#idea: investigate the extrem points, see if certain strains are specielizing in certain environmens

#first look at data properties - marker
head(marker)
dim(marker)
colnames(marker)
unique(marker$chrom)
marker[, length := (end-start+1)]

#max/min length
marker[, .(max = max(length), min = min(length))]

#plot distribution of lengths of all markers
ggplot(data = marker, aes(length)) + geom_density()

#idea: add length of marker and see if it correlates with any growth rate informmation
genotype_long[strain == "seg_02B" & value == "Lab strain"]


table_1 <- genotype_long[value == "Lab strain",.N/1000, by = strain]
table_2 <- growth[,.(strain,YPD, YPD_BPS, YPD_Rapa,YPE, YPMalt )]
table <- merge(table_1,table_2)

ggplot(data = table, aes(x=V1, y=YPMalt)) + geom_jitter()

# box plot
?which

mygeno <- genotype[, which(marker$chrom=="chr07" & marker$start== 1069229)]

