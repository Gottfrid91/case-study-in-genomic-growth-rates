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
marker <- read.delim(marker_file)
growth <- read.delim(growth_file)
genotype <- read.delim(genotype_file)

#analyze type of data object
typeof(marker)
str(marker)

dim(marker)
head(marker)

mygeno <- genotype[, which(marker$chrom=="chr07" & marker$start== 1069229)]
mygeno1 <- genotype[, which(marker$chrom=="chr01" & marker$start== 1512)+1]

names(mygeno) <- genotype$strain
names(mygeno1) <- genotype$strain

marker[which(marker$chrom=="chr07" & marker$start== 1069229),"id"]

test <- as.data.table(mygeno)
test[, strain := genotype$strain]
test[, mrk_id := marker[which(marker$chrom=="chr07" & marker$start== 1069229),"id"]]

growth_dt <- as.data.table(growth)

test_2 <- merge(test,growth_dt[,.(strain, YPMalt)])



test_2[,mean(YPMalt, na.rm = TRUE), by = .(mrk_id,mygeno)]

head(growth_dt[,.(YPMalt)])

plot(YPMalt ~ mygeno[strain], data=growth)
plot(YPMalt ~ mygeno1[strain], data=growth)


ggplot(data = growth , aes(x=mygeno, y=YPMalt)) + geom_boxplot() + geom_jitter()

##
genotype_dt <- as.data.table(genotype)
dim(genotype_dt)

base <- merge(genotype_dt, growth)

final_dt <- as.data.table(marker[1])
final_dt[, avg_lab := 0]
final_dt[, avg_wild := 0]

iterator <- lapply(final_dt[,1], as.character)
k <- 1
for(i in iterator){
  x <- base[, mean(YPD, na.rm = TRUE), by = eval(i)][,V1]
  print(x[k])
  print(x[k+1])
  final_dt[id == i, avg_lab := x[k]]
  final_dt[id == i, avg_wild := x[k+1]]
  k <- k + 1
}


k <- 1
for(i in 1:length(x)){
  print(x[k])
  k <- k +1
}

for(i in iterator){
  x <- base[, mean(YPD, na.rm = TRUE), by = eval("mrk_14")][,V1]  
  print(x)
}
x
colnames(base)[2]
?colMeans

as.list(marker[1])

## Compute row and column sums for a matrix:
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
rowSums(x); colMeans(x)
final_dt[id=="mrk_14",avg_lab := 8]

#make table long
base_2 <- base
base_2_long <- melt(base_2, id.vars = c("strain","YPD", "YPD_BPS", "YPD_Rapa", "YPE" ,"YPMalt"))
#adding all avarages in one table
avg_YPD <- base_2_long[,mean(YPD, na.rm = TRUE), by = .(variable, value)]
avg_YPD[,"AVG_APD_BPS" := base_2_long[,mean(YPD_BPS, na.rm = TRUE), by = .(variable, value)][,3]]
avg_YPD[,"AVG_YPD_Rapa" := base_2_long[,mean(YPD_Rapa, na.rm = TRUE), by = .(variable, value)][,3]]
avg_YPD[,"AVG_YPE" := base_2_long[,mean(YPE, na.rm = TRUE), by = .(variable, value)][,3]]
avg_YPD[,"AVG_YPMalt" := base_2_long[,mean(YPMalt, na.rm = TRUE), by = .(variable, value)][,3]]

setnames(avg_YPD, old = c("variable","value", "V1"), new=c("ID","Genome","AVG_YPD"))

#plot
ggplot(data=avg_YPD, aes(x=Genome, y=AVG_APD_BPS)) + geom_col()

#melt table

avg_long <- melt(avg_YPD,id.variable = "Nutrition", measure.name = 3:7)

ggplot(data=avg_long, aes(x=Genome, y=value)) + geom_boxplot() + facet_wrap(~variable)

avg_long[, mean(value, na.rm = TRUE), by = Genome]

#derive marker avarages and standard deviationn for each nutrition
avg_nutrition_markers <- avg_YPD[,.(mean(AVG_YPD),mean(AVG_APD_BPS),mean(AVG_YPD_Rapa), mean(AVG_YPE), mean(AVG_YPMalt)), by = Genome]
sd_nutrition_markers <- avg_YPD[,.(sd(AVG_YPD),sd(AVG_APD_BPS),sd(AVG_YPD_Rapa), sd(AVG_YPE), sd(AVG_YPMalt)), by = Genome]

setnames(sd_nutrition_markers, old = 2:6, new = c("sd(AVG_YPD)","sd(AVG_APD_BPS)","sd(AVG_YPD_Rapa)", "sd(AVG_YPE)", "sd(AVG_YPMalt"))
setnames(avg_nutrition_markers, old = 2:6, new = c("avg(AVG_YPD)","avg(AVG_APD_BPS)","avg(AVG_YPD_Rapa)", "avg(AVG_YPE)", "avg(AVG_YPMalt"))

#plot
avg_n_merkers_long <- melt(avg_nutrition_markers, id.vars = "Genome")
sd_n_merkers_long <- melt(sd_nutrition_markers, id.vars = "Genome")

ggplot(data = avg_n_merkers_long, aes(x=Genome, y=value)) + geom_col() + facet_wrap(~variable)

ggplot(data = avg_n_merkers_long, aes(x=variable, y=value,fill = Genome)) + geom_col(position = "dodge") + 
  geom_errorbar(aes(ymax=value + sd_n_merkers_long$value, ymin=value - sd_n_merkers_long$value), position = "dodge")
