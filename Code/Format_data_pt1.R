###########################################
# The purpose of this script is to 
# explore a dataset containing location (addresses) of hospitals
# in Chicago, the goal is to calculate distance to the nearest medical center
# for each community area (centroid)
# this measure will be used as a proxy for access to healthcare facilities 
###########################################

# load packages 
library(sp) #spatial data wrangling & analysis
library(sf) #spatial data wrangling & analysis

library(rgdal) #spatial data wrangling
library(rgeos) #spatial data wrangling & analytics
library(tidyverse) #data wrangling

library(tmap) #modern data visualizations
library(leaflet) #modern data visualizations
library(ggmap) #to obtain lat/lon coords from addresses 
library(geosphere) #to calculate distances
library(tidyr) #data wrangling
library(sjPlot) #create nice data tables
library(stargazer) #create nice data tables

# read data of Chicago hospital locations, last updated August 28, 2011
chicago.hospitals <- st_read("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Raw_Data/Hospitals/Hospitals.shp")
class(chicago.hospitals)
# check the projection information using st_crs
st_crs(chicago.hospitals)
# list the content of the data frame 
dat.for.table <- read.csv("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Generated_Data/Chicago_hospital_list_address.csv")
stargazer(dat.for.table, summary = FALSE, type = "html", title = "List of Chicago Hospitals", digits = 1, out = "table2.htm")

# remove rehabilitation centers from the list, as well as a neurology center 
chicago.hospitals.sub <- subset(chicago.hospitals, chicago.hospitals$FACILITY != "Schwab Rehabilitation Hospital" & chicago.hospitals$FACILITY != "Rehabilitation Institute of Chicago" &
                                  chicago.hospitals$FACILITY != "Chicago Institute of Neurosurgery and Neuroresearch")
# this should have remove 3 institutions, bringing the number of obs down to 39

# select variables of interest
chicago.hospitals.sub <- chicago.hospitals.sub[, c("FACILITY", "ADDRESS", "AREA_NUMBE", "COMMUNITY")]

# to convert addresses to points (lat & long) we will use a function of ggmap
# this function now requires a registered API key 
# to obtain an API key and enable services, go to https://cloud.google.com/maps-platform/

# this sets your google map permanently
# register_google(key = "AIzaSyA05W_1xRb01Wf7jALMGzbjveGvSL7oM1Q", write = TRUE) 
has_google_key()
google_key()

# obtain lat and long for each address, create a new dataset 
locs <- as.character(chicago.hospitals.sub$ADDRESS) # list of addresses 
hospital.coords <- geocode(locs) # ggmap function 
hospital.coords$address <- locs # adds addresses to dataset 

# the next step is to obtain shape centers for each of our community areas
# read data of boundaries for current community areas in Chicago
chicago.comm <- read_sf("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Raw_Data/Boundaries_Community_Areas_current/current_boundaries_community_areas.geojson")
class(chicago.comm)
# check the projection information using st_crs
st_crs(chicago.comm)
# the layer is unprojected in decimal degrees. Also, a quick plot
# note that, by default, sf draws a choropleth map for each variable included in the data frame
# since we won’t be using sf for mapping, we ignore that aspect for now
plot(chicago.comm)

# ensure area numbers are read as integer
class(chicago.comm$area_num_1)
chicago.comm$area_num_1 <- as.integer(chicago.comm$area_num_1)

# obtain shape centers 
# the st_centroid function is part of sf (there is no obvious counterpart to the mean center functionality) 
# it creates a point simple features layer and contains all the variables of the original layer
chicago.centroid <- st_centroid(chicago.comm)

# explore results visually  
setwd("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Figures")
pdf("Chicago_comm_area_centroids_and_hospital_locations.pdf", height = 8, width = 15) 
# plot centroid locations (red)
plot1 <- tm_shape(chicago.comm) +
  tm_borders() +
  tm_shape(chicago.centroid) +
  tm_dots(size = 0.2, col = "red")
# plot hospital locations (blue)
plot2 <- plot1 +
  tm_shape(chicago.hospitals.sub) +
  tm_dots(size = 0.2, col = "blue") +
  tm_layout(main.title = "Distribution of Hospitals Across Chicago",
            main.title.position = "left", main.title.size = 0.85)
# add legend 
plot3 <- plot2 +
  tm_add_legend(
    type = c("fill", "symbol", "text", "line"),
    labels = c("shape center", "hospital"),
    col = c("red", "blue"),
    size = 0.60,
    shape = NULL,
    lwd = NULL,
    lty = 0.45,
    text = 0.45,
    alpha = NA,
    border.col = "black",
    border.lwd = 1,
    border.alpha = NA,
    title = "Locations",
    is.portrait = TRUE,
    legend.format = list(),
    reverse = FALSE,
    z = NA,
    group = NULL
  ) +
  # adjust legend 
  tm_layout(legend.position = c("left", "center"), legend.title.size = 0.95)

print(plot3)
dev.off()

# save new formatted dataset as a shapefile
setwd("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Generated_Data")
st_write(chicago.centroid, "chicago_comm_area_centroids", driver = "ESRI Shapefile")

# obtain lat and long coordinates for the different centroids 
centroid_coords <- do.call(rbind, st_geometry(chicago.centroid)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
# add comunity area information (name and area number)
centroid_coords$community <- chicago.centroid$community
centroid_coords$area_number <- chicago.centroid$area_num_1

# calculate distance to nearest hospital using 'Haversine' Great Circle Distance
# the shortest distance between two points (i.e., the 'great-circle-distance' or 'as the crow flies'), according to the 'haversine method'
# this method assumes a spherical earth, ignoring ellipsoidal effects. As default, estimates are in meters.

# obtains list of names to run loop
community.area.list <- unique(centroid_coords$community)
hospital.list <- unique(hospital.coords$address)
# create new matrix to save values 
to_save <- as.data.frame(matrix(NA, nrow = length(community.area.list), ncol = 3))
colnames(to_save) <- c("community","closest.hospital.address","distance.to.hospital.km")

for (i in 1:nrow(centroid_coords)) {
  # select a community area
  current_comm_area <- community.area.list[i]
  # select the data for that area
  current_comm_area_dat <- filter(centroid_coords, centroid_coords$community == current_comm_area)
  # save the lat and long for that community area 
  comm_area_lat <- current_comm_area_dat$lat
  comm_area_lon <- current_comm_area_dat$lon
  
  # create empty vector to store calculated distances
  distance_vect <- rep(NA, nrow(geocoded))
  
  for (j in 1:nrow(hospital.coords)) {
    # for every hospital on the list 
    current_hospital <- hospital.list[j]
    # obtains it data 
    current_hospital_dat <- filter(hospital.coords, hospital.coords$address == current_hospital)
    # save its coordinates 
    current_hospital_lat <- current_hospital_dat$lat
    current_hospital_lon <- current_hospital_dat$lon
    
    # calculate distance to current hospital from community area centroid 
    distance_to_current_hospital <- distm(c(comm_area_lon, comm_area_lat),
                                          c(current_hospital_lon, current_hospital_lat), fun = distHaversine)
    # save it in vector, results are in meters 
    distance_vect[j]  <- distance_to_current_hospital
  }
   # obtain the shortest distance to hospital
   shortest_dist_to_hospital <- distance_vect[which(distance_vect == min(distance_vect))]
   shortest_dist_to_hospital_km <- shortest_dist_to_hospital/1000
   # save values to dataset 
   to_save$community[i] <- current_comm_area
   to_save$closest.hospital.address[i] <- hospital.list[which(distance_vect == min(distance_vect))]
   to_save$distance.to.hospital.km[i] <- round(shortest_dist_to_hospital_km, digits = 2)
}

# save data in a new csv 
setwd("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Generated_Data")
write.csv(to_save, "km_distance_to_nearest_hospital_chicago_comm_area_dat_2011.csv", row.names = FALSE)
