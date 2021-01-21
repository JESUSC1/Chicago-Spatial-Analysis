###########################################
# The purpose of this script is to 
# consolidate data from three diff datasets
# two which were obtained from the Chicago Data Portal 
# in order to create a simple features spatial object
# The hope for this is to create a simple pipeline
# to analyze future spatial data 
# for tutorial see https://spatialanalysis.github.io/lab_tutorials/1_R_Spatial_Data_Handling.html 
###########################################

# load packages 
library(sp) #spatial data wrangling & analysis
library(sf) #spatial data wrangling & analysis

library(rgdal) #spatial data wrangling
library(rgeos) #spatial data wrangling & analytics
library(tidyverse) #data wrangling

library(tmap) #modern data visualizations
library(leaflet) #modern data visualizations

# set working directory
setwd("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Raw_Data")

# read and inspect mortality data by underlying cause of death, Chicago 2006-2010 by community area
mort.dat <- read.csv("Public_Health_Statistics_selected_underlying_causes_of_death_in_Chicago_2006_2010.csv")
head(mort.dat)
# subset data to include only variables of interest
mort.dat.sub <- mort.dat %>% select(Community.Area.Name, area_num_1 = Community.Area, cause_of_death = Cause.of.Death,
                                         average_adjusted_rate = Average.Adjusted.Rate.2006...2010, Adjusted.Rate.Rank)
# set comm names as uppercase
mort.dat.sub$Community.Area.Name <- toupper(mort.dat.sub$Community.Area.Name)
# and cause of death as lowercase 
mort.dat.sub$cause_of_death <- tolower(mort.dat.sub$cause_of_death)
# ensure area numbers are read as integer
class(mort.dat.sub$area_num_1)

# subset data to include the underlying causes of death for which we are more interested in
mort.dat.sub <- filter(mort.dat.sub, cause_of_death == "injury, unintentional" | cause_of_death == "coronary heart disease" |
                    cause_of_death == "diabetes-related" | cause_of_death == "stroke (cerebrovascular disease)")

# remove data for Chicago
mort.dat.sub <- subset(mort.dat.sub, mort.dat.sub$Community.Area.Name != "CHICAGO")
comm.list <- unique(mort.dat.sub$Community.Area.Name)
length(comm.list) # should be 77 

# set column names to uppercase to match socieconomic data
names(mort.dat.sub) <- toupper(names(mort.dat.sub))

# read and inspect dataset for selected socioeconomic indicators in Chicago 
# this dataset contains a selection of six socioeconomic indicators of public health
# significance and a "hardship index" by Chicago community area, for the years 2008-2012
socioeconomic.dat <- read.csv("Census_Data__selected_socioeconomic_indicators_Chicago_2008_2012.csv")
head(socioeconomic.dat)
# remove data for all of Chicago
socioeconomic.dat  <- na.omit(socioeconomic.dat )
socioeconomic.dat$COMMUNITY.AREA.NAME <- toupper(socioeconomic.dat$COMMUNITY.AREA.NAME) #change names to uppercase to match the other dataset
# select only socioeconomic variables of interes
socioeconomic.dat <- socioeconomic.dat[, c("COMMUNITY.AREA.NAME", "HARDSHIP.INDEX", "PER.CAPITA.INCOME")]

# add data of distance to nearest (km)
hospit.dis.dat <- read.csv("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Generated_Data/km_distance_to_nearest_hospital_chicago_comm_area_dat_2011.csv")
colnames(hospit.dis.dat) <- c("COMMUNITY.AREA.NAME", "HOSPITAL.ADDRESS", "DISTANCE.NEAREST.HOSPITAL")
hospit.dis.dat$COMMUNITY.AREA.NAME <- toupper(hospit.dis.dat$COMMUNITY.AREA.NAME) #change names to uppercase to match the other dataset
hospit.dis.dat <- hospit.dis.dat[, c(1,3)] #select variables of interest 
# recode mispelled community area name 
hospit.dis.dat$COMMUNITY.AREA.NAME[hospit.dis.dat$COMMUNITY.AREA.NAME == "MONTCLARE"] <- "MONTCLAIRE"

# merge socioeconomic data 
soc.eco.dat <- left_join(socioeconomic.dat, hospit.dis.dat, by = "COMMUNITY.AREA.NAME")
# recode mispelled community area name 
soc.eco.dat$COMMUNITY.AREA.NAME[soc.eco.dat$COMMUNITY.AREA.NAME == "WASHINGTON HEIGHT"] <- "WASHINGTON HEIGHTS"

# merge dependent and independent variable data
dat <- left_join(mort.dat.sub, soc.eco.dat , by = "COMMUNITY.AREA.NAME")
summary(dat) #there should not be any NA's

# recode community area name to match those in boundary data file
dat$COMMUNITY.AREA.NAME[dat$COMMUNITY.AREA.NAME == "O'HARE"] <- "OHARE"
dat$COMMUNITY.AREA.NAME[dat$COMMUNITY.AREA.NAME == "MONTCLAIRE"] <- "MONTCLARE"

# read data of boundaries for current community areas in Chicago
chicago.comm <- read_sf("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Raw_Data/Boundaries_Community_Areas_current/current_boundaries_community_areas.geojson")
class(chicago.comm)
# check the projection information using st_crs
st_crs(chicago.comm)
# the layer is unprojected in decimal degrees. Also, a quick plot
# note that, by default, sf draws a choropleth map for each variable included in the data frame
# since we won’t be using sf for mapping, we ignore that aspect for now
plot(chicago.comm)

# changing projections, we will assign the Universal Tranverse Mercator zone 16N, which is
# the proper one for Chicago, with an ESPG code of 32616
chicago.comm <- st_transform(chicago.comm, 32616)
st_crs(chicago.comm) #check the result 

# ensure area numbers are read as integer
class(chicago.comm$area_num_1)
chicago.comm$area_num_1 <- as.integer(chicago.comm$area_num_1)

# spatial join data 
to.save <- left_join(chicago.comm, dat, by = c("community" = "COMMUNITY.AREA.NAME", "area_num_1" = "AREA_NUM_1"))
# change all column names to lowercase 
names(to.save) <- tolower(names(to.save))

# basic chloropleth map
tm_shape(to.save) +
  tm_polygons("per.capita.income") #there should not be any NA's

# explore data for diabetes-related deaths 
diabetes.dat <- filter(to.save, cause_of_death == "diabetes-related")
head(diabetes.dat)

# create plot 
setwd("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Figures")
pdf("diabetes_related_deaths_age_adjusted_Chicago_2006_2010.pdf", height = 8, width = 15) 
tm_shape(diabetes.dat) +
  tm_polygons("average_adjusted_rate", title = "average adjusted rate", legend.hist = FALSE, legend.hist.z = 0) +
  tm_layout(inner.margins = 0, 
            legend.text.size = .95,
            legend.title.size = .97,
            legend.position = c("left","bottom"),
            legend.hist.height = .2, legend.hist.width = .3)
dev.off()  
# for futher information on any of the following aspects:
# reading and loading a shapefile
# creating choropleth maps for different classifications
# customizing choropleth maps
# selecting appropriate color schemes
# calculating and plotting polygon centroids
# omposing conditional maps
# creating a cartogram
# see the following link:
# https://spatialanalysis.github.io/lab_tutorials/4_R_Mapping.html
  
# save new formatted dataset as a shapefile
setwd("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Generated_Data")
st_write(to.save,"selected_underlying_cause_of_death_dat_plus_socioeconomic_variables_Chicago_2006_2010", driver = "ESRI Shapefile")

# create shapefiles for each underlying cause of death 
to.save.hd <- filter(to.save, cause_of_death == "coronary heart disease")
st_write(to.save.hd,"coronary_heart_disease", driver = "ESRI Shapefile")

to.save.dr <- filter(to.save, cause_of_death == "diabetes-related")
st_write(to.save.dr,"diabetes_related", driver = "ESRI Shapefile")

to.save.st <- filter(to.save, cause_of_death == "stroke (cerebrovascular disease)")
st_write(to.save.st,"stroke_cerebrovascular_disease", driver = "ESRI Shapefile")

to.save.injury <- filter(to.save, cause_of_death == "injury, unintentional")
st_write(to.save.injury,"injury_unintentional", driver = "ESRI Shapefile")
