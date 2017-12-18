#case study I
#load packages
library(data.table)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(gridExtra)

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
medians <- base_2_long[, lapply(.SD, median, na.rm = TRUE),
                       .SDcols = c("YPD", "YPD_BPS", "YPD_Rapa", "YPE", "YPMalt"),
                       by = .(marker = variable, genome = value)]

medians_diff <- medians[, lapply(.SD, subtract),
                            .SDcols = c("YPD"),
                            by = marker]

ypd_diff <- medians[genome == "Wild isolate", YPD := -1 * YPD][, .(YPD = sum(YPD)), marker]
ypd_bps_diff <- medians[genome == "Wild isolate", YPD_BPS := -1 * YPD_BPS][, .(YPD_BPS = sum(YPD_BPS)), marker]
ypd_rapa_diff <- medians[genome == "Wild isolate", YPD_Rapa := -1 * YPD_Rapa][, .(YPD_Rapa = sum(YPD_Rapa)), marker]
ype_diff <- medians[genome == "Wild isolate", YPE := -1 * YPE][, .(YPE = sum(YPE)), marker]
ypmalt_diff <- medians[genome == "Wild isolate", YPMalt := -1 * YPMalt][, .(YPMalt = sum(YPMalt)), marker]

# markers grid plot
plots = lapply(392:400, function(.x) ggplot(growth, aes((genotype[, .x])[strain], YPMalt)) + geom_boxplot())
do.call(grid.arrange,  plots)
