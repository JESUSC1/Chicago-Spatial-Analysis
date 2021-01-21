###########################################
# The purpose of this script is to 
# carry out preliminary data exploration
# on our dependent variables, using some of the methods employed in
# the Exploratory Data Analysis I tutorial 
# https://spatialanalysis.github.io/lab_tutorials/2_R_EDA_1.html
###########################################

# load packages 
library(sp) #spatial data wrangling & analysis
library(sf) #spatial data wrangling & analysis

library(rgdal) #spatial data wrangling
library(rgeos) #spatial data wrangling & analytics
library(tidyverse) #data wrangling

library(tmap) #modern data visualizations
library(leaflet) #modern data visualizations
library(Hmisc) #to utilize the LOWESS smoother
library(sjPlot) #to create nice tables 
library(stargazer) #to create nice tables 
library(Hmisc) #for correlation matrix with p-values
library(corrplot) #to visualize correlation matrix 
library(PerformanceAnalytics) #for correlation chart

# set working directory
setwd("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Generated_Data/selected_underlying_cause_of_death_dat_Chicago_2006_2010")
# read and inspect mortality data by underlying cause of death, Chicago 2006-2010 by community area
dat <- read_sf("selected_underlying_cause_of_death_dat_Chicago_2006_2010.shp")
head(dat)

# select data for specific causes of death:
# injury, unintentional; coronary heart disease; diabetes-related; stroke (cerebrovascular disease)
dat.sub <- filter(dat, cs_f_dt == "injury, unintentional" | cs_f_dt == "coronary heart disease" |
                    cs_f_dt == "diabetes-related" | cs_f_dt == "stroke (cerebrovascular disease)")
head(dat.sub)
causes.list <- unique(dat.sub$cs_f_dt)
print(causes.list) # there should only be four causes of death!!

# remove data for Chicago
dat.sub <- filter(dat.sub, comm != "CHICAGO")
comm.list <- unique(dat.sub$comm)
length(comm.list) # there should be 77 community areas 

# analyze the distribution of the average number of deaths for each different cause 
# interleaved histograms
ggplot(dat.sub, aes(x = avrg_d_ ,color = cs_f_dt )) +
  geom_histogram(fill = "white", position = "dodge", bins = 55) +
  theme(legend.position = "top")
# ddd mean lines
library(plyr)
dat.mean <- ddply(dat.sub, "cs_f_dt", summarise, grp.mean = mean(avrg_d_))
head(dat.mean)

p1 <- ggplot(dat.sub, aes(x = avrg_d_ , color = cs_f_dt)) +
  geom_histogram(fill = "white", position = "dodge", bins = 55) +
  geom_vline(data = dat.mean, aes(xintercept = grp.mean, color = cs_f_dt),
             linetype = "dashed") +
  theme(legend.position = "top") +
  xlab("Average Annual Death Rate (age-adjusted)")
  
p2 <- p1 + labs(fill = "Cause of Death")
p2
# distributions differ greatly by cause of death!!!

# create a boxplot 
base.plt <- ggplot(data = dat.sub, aes(x = cs_f_dt, y = avrg_d_))
base.plt + geom_boxplot() +
  xlab("") +
  ggtitle("") +
  xlab("Cause of Death") + ylab("Average Annual Death Rate (age-adjusted)") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(face = "bold", color = "black", 
                                                                           size = 10, angle = 0))
# another version 
setwd("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Figures")
pdf("boxplot_underlying_cause_of_death_Chicago_2006_2010.pdf", height = 8, width = 12) 
base.plt + 
  geom_point(color = "black", alpha = 0.5) +
  geom_boxplot(color = "black", fill = "grey", outlier.color = "red", alpha = 0.5) +
  stat_boxplot(geom = "errorbar") +
  xlab("") +
  ggtitle("") +
  xlab("Cause of Death") + ylab("Average Annual Death Rate (age-adjusted)") 
dev.off()

# create descriptive statistics table for dependent variables 
# subset data for table
table.dat <- dat.sub[, c("commnty", "cs_f_dt", "avrg_d_")]
# summarize data 
table.dat.sum <- to.save %>%
  group_by(cs_f_dt) %>%
  summarize(mean = round(mean(avrg_d_), digits = 0), SD = round(sd(avrg_d_), digits = 0), median = round(median(avrg_d_), digits = 0),
            min = round(min(avrg_d_), digits = 0), max = round(max(avrg_d_), digits = 0))
# rename column
table.dat.sum <- table.dat.sum %>% 
  rename(Cause.of.Death = cs_f_dt)
# create table
tab_df(table.dat.sum,
       title = "Descriptive statistics for dependent variables", 
       file = "table2_sum_stats_dpv.doc")

# bivariate analysis: the scatter plot 
dat.sub <- dat.sub %>% 
  rename(Cause.of.Death = cs_f_dt) #rename column

setwd("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Figures")
pdf("scatter_plot_underlying_cause_of_death_YPLL_Chicago_2006_2010.pdf", height = 8, width = 12) 
p3 <- ggplot(data = dat.sub, aes(x = avrg_d_, y = YPLL, color = Cause.of.Death)) +
  geom_point() +
  xlab("Average Annual Death Rate (age-adjusted)") +
  ylab("Average Annual Years of Potential Life Lost \n(per 100,000 residents) ") +
  ggtitle("") 
#coord_fixed(ratio = 55.0/25.0)
print(p3)
dev.off()
# comparison of smoothing methods 
ggplot(data = dat.sub, aes(x = avrg_d_, y = YPLL_rn)) +
  stat_plsmo(aes(color = "lowess")) +
  geom_point() +
  geom_smooth(aes(color = "lm"), method = lm, se = FALSE) +
  geom_smooth(aes(color = "loess"), method = loess,se = FALSE) +
  xlab("Average Number of Deaths lost to Diabetes-related illnesses") +
  ylab("Average Annual Years of Possible Life Lost (rank)") +
  ggtitle("Comparison of Smoothing Methods") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "Method")

# Note, I need to find a way to differentiate among community areas (maybe North vs South?)
# in order to carry out tests of breaks in spatial heterogeneity 
