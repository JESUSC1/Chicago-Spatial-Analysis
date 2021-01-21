###########################################
# The purpose of this script is to 
# carry out preliminary data exploration
# on some of the independent variables from our dataset using some of the methods employed in
# the Exploratory Data Analysis II tutorial 
# https://spatialanalysis.github.io/lab_tutorials/3_R_EDA_2.html
###########################################

# load packages 
library(tidyverse) #data wrangling
library(GGally) #add-on package to create a scatterplot matrix and parallel coordinate plot
library(scatterplot3d) #to create a static 3d scatter plot
library(plotly) #to construct interactive 3d scatter and parallel coordinate plots 
library(Hmisc) #for correlation matrix with p-values
library(corrplot) #to visualize correlation matrix 
library(PerformanceAnalytics) #for correlation chart

# set working directory
setwd("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Raw_Data")
# read and inspect dataset for selected socioeconomic indicators in Chicago 
# this dataset contains a selection of six socioeconomic indicators of public health
# significance and a "hardship index" by Chicago community area, for the years 2008-2012
socioeconomic.dat <- read.csv("Census_Data__selected_socioeconomic_indicators_Chicago_2008_2012.csv")
head(socioeconomic.dat)
# remove data for all of Chicago
socioeconomic.dat  <- na.omit(socioeconomic.dat )
socioeconomic.dat$COMMUNITY.AREA.NAME <- toupper(socioeconomic.dat$COMMUNITY.AREA.NAME) #change names to uppercase to match the other dataset

# add data of distance to nearest (km)
hospit.dis.dat <- read.csv("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Generated_Data/km_distance_to_nearest_hospital_chicago_comm_area_dat_2011.csv")
colnames(hospit.dis.dat) <- c("COMMUNITY.AREA.NAME", "HOSPITAL.ADDRESS", "DISTANCE.NEAREST.HOSPITAL")
hospit.dis.dat$COMMUNITY.AREA.NAME <- toupper(hospit.dis.dat$COMMUNITY.AREA.NAME) #change names to uppercase to match the other dataset

# merge datasets 
dat <- left_join(socioeconomic.dat, hospit.dis.dat, by = "COMMUNITY.AREA.NAME")
# shorten column names 
names(dat)
colnames(dat) <- c("AREA.N", "COMMUNITY","PHC", "PHBP", "PA16U", "PA25WHS", "PAU18OV64", "PCI", "HI", "HOSPA", "DNH")

# arrange data for correlation matrix 
table.dat <- dat[, -c(1,2, 10)]

# basic scatter plot matrix 
ggscatmat(table.dat)
# another version 
ggpairs(table.dat)

# correlation tests for independent variables 
res <- cor(table.dat, method = "pearson", use = "complete.obs")
round(res, 2)

# correlation with significance values 
res2 <- rcorr(as.matrix(table.dat), type = "pearson")
res2

# extract the correlation coefficients
res2$r
# extract p-values
res2$P

# visualize correlation
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

setwd("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Figures")
pdf("corr_plot_independent_variables_.pdf", height = 8, width = 12) 
# insignificant correlation are crossed
corrplot(res2$r, type = "upper", order = "hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")
# insignificant correlations are leaved blank
corrplot(res2$r, type = "upper", order = "hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")
dev.off()

?corrplot()

# use heat map 
col <- colorRampPalette(c("blue", "white", "red"))(20) # get some colors
heatmap(x = res, col = col, symm = TRUE)

# use correlation chart 
pdf("corr_chart_independent_variables_.pdf", height = 8, width = 12) 
chart.Correlation(table.dat, histogram = TRUE, pch = 19)
dev.off()

# to interpret its value, see which of the following values your correlation r is closest to: 
# exactly –1. A perfect downhill (negative) linear relationship
# –0.70. A strong downhill (negative) linear relationship
# –0.50. A moderate downhill (negative) relationship
# –0.30. A weak downhill (negative) linear relationship  
# 0. No linear relationship
# 0.30. A weak uphill (positive) linear relationship
# 0.50. A moderate uphill (positive) relationship
# 0.70. A strong uphill (positive) linear relationship
# exactly +1. A perfect uphill (positive) linear relationship  
  
# pairwise scatter plots 
ggpairs(dat, columns = c("PERCENT.OF.HOUSING.CROWDED","PERCENT.HOUSEHOLDS.BELOW.POVERTY",
                         "PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA", "PER.CAPITA.INCOME", "HARDSHIP.INDEX"),
        upper = list(continuous = "points"), diag = list(continuous = "barDiag"))

# scatter plot matrix with loess smoother
ggpairs(dat, columns = c("PERCENT.OF.HOUSING.CROWDED","PERCENT.HOUSEHOLDS.BELOW.POVERTY",
                        "PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA", "PER.CAPITA.INCOME", "HARDSHIP.INDEX"),
        upper = list(continuous = "smooth_loess"), lower = list(continuous = "smooth_loess"))


# bubble chart
setwd("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Figures")
pdf("bubble_chart.pdf", height = 8, width = 12) 
ggplot(data = dat, aes(x = PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA, y = PERCENT.OF.HOUSING.CROWDED, size = HARDSHIP.INDEX, col = PER.CAPITA.INCOME)) +
  geom_point() +
  xlab("% of individuals aged 25 without HS Diploma") +
  ylab("% of Housing Crowded") +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

# bubble chart V2
pdf("bubble_chart2.pdf", height = 8, width = 12) 
ggplot(data = dat, aes(x = PERCENT.OF.HOUSING.CROWDED, y = PERCENT.AGED.UNDER.18.OR.OVER.64  , size = HARDSHIP.INDEX, col = PER.CAPITA.INCOME)) +
  geom_point() +
  xlab("% of Housing Crowded") +
  ylab("% of the population under 18 or over 64 years of age") +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

# parallel coordinate plot (PCP)
vars <- c("PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA","PERCENT.OF.HOUSING.CROWDED",
          "PER.CAPITA.INCOME","HARDSHIP.INDEX")
pcp.vars <- select(dat,vars)
ggparcoord(data = pcp.vars)
# consider reordering the columns to minimize the number of cross between series
# scaling the variables might also make comparisons easier 


# create conditional plot with LOESS Smoother 
# the facetting formula does not evaluate functions, so the conditioning categories need to be computed beforehand
# there are three so-called helper functions to make this easy: cut_interval, cut_width, and cut_number
# the closest to the median (2 quantiles) conditioning illustrated in the GeoDa Workbook is the cut_number function
# variable will be split in two groups on the median value
dat$xcut <- cut_number(dat$PERCENT.OF.HOUSING.CROWDED, n = 2) # condition variable for the x-axis
dat$ycut <- cut_number(dat$PER.CAPITA.INCOME, n = 2) # condition variable for the y-axis


ggplot(data = dat, aes(x = PERCENT.AGED.UNDER.18.OR.OVER.64 , y = HARDSHIP.INDEX )) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_grid(ycut ~ xcut, as.table = FALSE) # make sure to have the right order!!

