#case study I
#load packages
library(data.table)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(gridExtra)
library(dplyr)

#set data dir
DATA_DIR <- c('/home/salma/Desktop/case-study-1/')

marker_file <- file.path(DATA_DIR, 'marker.txt')
growth_file <- file.path(DATA_DIR, 'growth.txt')
genotype_file <- file.path(DATA_DIR, 'genotype.txt')

#read in data
marker <- read.delim(marker_file)
growth <- read.delim(growth_file)
genotype <- read.delim(genotype_file)

#convert to data table
growth_dt <- as.data.table(growth)
genotype_dt <- as.data.table(genotype)

#merge for data for later plotting
base <- merge(genotype_dt, growth)
base_long <- melt(base, id.vars = c("strain","YPD", "YPD_BPS", "YPD_Rapa", "YPE" ,"YPMalt"))
#adding all avarages in one table
avg <- base_long[,mean(YPD, na.rm = TRUE), by = .(variable, value)]
avg[,"AVG_APD_BPS" := base_long[,mean(YPD_BPS, na.rm = TRUE), by = .(variable, value)][,3]]
avg[,"AVG_YPD_Rapa" := base_long[,mean(YPD_Rapa, na.rm = TRUE), by = .(variable, value)][,3]]
avg[,"AVG_YPE" := base_long[,mean(YPE, na.rm = TRUE), by = .(variable, value)][,3]]
avg[,"AVG_YPMalt" := base_long[,mean(YPMalt, na.rm = TRUE), by = .(variable, value)][,3]]

#set names for data columns
setnames(avg, old = c("variable","value", "V1"), new=c("ID","Genome","AVG_YPD"))

#melt table

avg_long <- melt(avg_YPD,id.variable = "Nutrition", measure.name = 3:7)

avg_long[, mean(value, na.rm = TRUE), by = Genome]

#derive marker avarages and standard deviationn for each nutrition
avg_nutrition_markers <- avg[,.(mean(AVG_YPD),mean(AVG_APD_BPS),mean(AVG_YPD_Rapa), mean(AVG_YPE), mean(AVG_YPMalt)), by = Genome]
sd_nutrition_markers <- avg[,.(sd(AVG_YPD),sd(AVG_APD_BPS),sd(AVG_YPD_Rapa), sd(AVG_YPE), sd(AVG_YPMalt)), by = Genome]

setnames(sd_nutrition_markers, old = 2:6, new = c("AVG_YPD","AVG_APD_BPS","AVG_YPD_Rapa", "AVG_YPE", "AVG_YPMalt"))
setnames(avg_nutrition_markers, old = 2:6, new = c("AVG_YPD","AVG_APD_BPS","AVG_YPD_Rapa", "AVG_YPE", "AVG_YPMalt"))

#plot
avg_n_merkers_long <- melt(avg_nutrition_markers, id.vars = "Genome")
sd_n_merkers_long <- melt(sd_nutrition_markers, id.vars = "Genome")

#set style
outlook <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

#plot
ggplot(data = avg_n_merkers_long, aes(x=variable, y=value,fill = Genome)) + geom_col(position = "dodge") + 
  geom_errorbar(aes(ymax=value + sd_n_merkers_long$value, ymin=value - sd_n_merkers_long$value), position = "dodge") +
  outlook +ylab("Growth") + xlab("") + ggtitle("Average growth for all markers per environment and genome")

# markers/genome medians
medians <- base_long[, lapply(.SD, median, na.rm = TRUE),
                       .SDcols = c("YPD", "YPD_BPS", "YPD_Rapa", "YPE", "YPMalt"),
                       by = .(marker = variable, genome = value)]

medians$genome <- gsub(" ", "_", medians$genome)

ypd_diff <- dcast(medians, marker ~ genome, value.var = "YPD") %>%
  mutate(Diff = Wild_isolate - Lab_strain) %>% 
  select(marker, Diff)
ypd_diff$Diff = abs(ypd_diff$Diff)
ypd_diff <- as.data.table(ypd_diff)

ypd_bps_diff <- dcast(medians, marker ~ genome, value.var = "YPD_BPS") %>%
  mutate(Diff = Wild_isolate - Lab_strain) %>% 
  select(marker, Diff)
ypd_bps_diff$Diff = abs(ypd_bps_diff$Diff)
ypd_bps_diff <- as.data.table(ypd_bps_diff)

ypd_rapa_diff <- dcast(medians, marker ~ genome, value.var = "YPD_Rapa") %>%
  mutate(Diff = Wild_isolate - Lab_strain) %>% 
  select(marker, Diff)
ypd_rapa_diff$Diff = abs(ypd_rapa_diff$Diff)
ypd_rapa_diff <- as.data.table(ypd_rapa_diff)

ype_diff <- dcast(medians, marker ~ genome, value.var = "YPE") %>%
  mutate(Diff = Wild_isolate - Lab_strain) %>% 
  select(marker, Diff)
ype_diff$Diff = abs(ype_diff$Diff)
ype_diff <- as.data.table(ype_diff)

ypmalt_diff <- m<- dcast(medians, marker ~ genome, value.var = "YPMalt") %>%
  mutate(Diff = Wild_isolate - Lab_strain) %>% 
  select(marker, Diff)
ypmalt_diff$Diff = abs(ypmalt_diff$Diff)
ypmalt_diff <- as.data.table(ypmalt_diff)

# compute affected markers
markers_affected_by_genotype <- ypd_bps_diff[Diff > 5.36*mean(Diff)]
setnames(markers_affected_by_genotype, "marker", "id")

query_markers <- inner_join(marker, markers_affected_by_genotype, by="id")

# markers grid plot
plots = lapply(query_markers$id,
               function(.x) ggplot(growth, aes(( genotype[, .x])[strain], YPD_BPS)) + geom_boxplot() + xlab("Genotype") + ggtitle(sub("mrk_", "marker ",.x)))
do.call(grid.arrange,  plots)

