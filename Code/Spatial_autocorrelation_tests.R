###########################################
# The purpose of this script is to 
# test for local spatial autocorrelation within our dataset 
# for tutorial see https://spatialanalysis.github.io/lab_tutorials/4_R_Mapping.html
###########################################

# load packages 
library(sp) #spatial data wrangling & analysis
library(sf) #spatial data wrangling & analysis

library(rgdal) #spatial data wrangling
library(rgeos) #spatial data wrangling & analytics
library(tidyverse) #data wrangling

library(tmap) #modern data visualizations
library(leaflet) #modern data visualizations

library(tmap) #for mapping
library(RColorBrewer) #to create colors
library(cartogram) #for mapping

############################## 
# LOAD DATA
##############################
dat <- st_read("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Generated_Data/selected_underlying_cause_of_death_dat_plus_socioeconomic_variables_Chicago_2006_2010/selected_underlying_cause_of_death_dat_plus_socioeconomic_variables_Chicago_2006_2010.shp")

# inspect column names
names(dat)

# basic chloropleth map example
tm_shape(dat) +
  tm_polygons("pr_cpt_") #there should not be any NA's
