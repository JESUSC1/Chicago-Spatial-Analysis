###########################################
# The purpose of this script is to 
# create geo-visualizations for our data 
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

############################## 
# BOX MAP
############################## 

# function to get variables from shapefile 
get.var <- function(vname, df) {
  # function to extract a variable as a vector out of an sf data frame
  # arguments:
  #    vname: variable name (as character, in quotes)
  #    df: name of sf data frame
  # returns:
  #    v: vector with values (without a column name)
  v <- df[vname] %>% st_set_geometry(NULL)
  v <- unname(v[,1])
  return(v)
}

# function to computete the box map break points 
boxbreaks <- function(v, mult = 1.5) {
  # break points for box map
  # arguments:
  #   v: vector with observations
  #   mult: multiplier for IQR (default 1.5)
  # returns:
  #   bb: vector with 7 break points
  # compute quartile and fences
  qv <- unname(quantile(v))
  iqr <- qv[4] - qv[2]
  upfence <- qv[4] + mult * iqr
  lofence <- qv[2] - mult * iqr
  # initialize break points vector
  bb <- vector(mode = "numeric", length = 7)
  # logic for lower and upper fences
  if (lofence < qv[1]) {  # no lower outliers
    bb[1] <- lofence
    bb[2] <- floor(qv[1])
  } else {
    bb[2] <- lofence
    bb[1] <- qv[1]
  }
  if (upfence > qv[5]) { # no upper outliers
    bb[7] <- upfence
    bb[6] <- ceiling(qv[5])
  } else {
    bb[6] <- upfence
    bb[7] <- qv[5]
  }
  bb[3:5] <- qv[2:4]
  return(bb)
}

# fucntion to plot boxmap
boxmap <- function(vnam, df, legtitle = NA, mtitle = "Box Map", mult = 1.5) {
  # box map
  # arguments:
  #   vnam: variable name (as character, in quotes)
  #   df: simple features polygon layer
  #   legtitle: legend title
  #   mtitle: map title
  #   mult: multiplier for IQR
  # returns:
  #   a tmap-element (plots a map)
  var <- get.var(vnam, df)
  bb <- boxbreaks(var)
  tm_shape(df) +
    tm_fill(vnam,title = legtitle,breaks = bb,palette = "-RdBu",
            labels = c("lower outlier", "< 25%", "25% - 50%", "50% - 75%","> 75%", "upper outlier"))  +
    tm_borders() +
    tm_layout(title = mtitle, title.position = c("right","bottom"))
}

#explore box plot for main independent variables 
plot_1 <- boxmap("pr_cpt_", dat , mtitle = "", legtitle = "per capita income", mult = 1.5) +
  tm_layout(title = "Per Capita Income", title.position = c("left","top"))
plot_2 <- boxmap("hrdshp_", dat , mtitle = "", legtitle = "hardship index", mult = 1.5) +
  tm_layout(title = "Hardship Index", title.position = c("left","top"))
plot_3 <- boxmap("dstnc__", dat , mtitle = "", legtitle = "kilometers", mult = 1.5) +
  tm_layout(title = "Distance to Nearest Hospital", title.position = c("left","top"))

# explore data for coronary heart disease 
hdisease.dat <- filter(dat, cs_f_dt == "coronary heart disease")
# box map for mortality rates 
setwd("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Figures")
pdf("box_map_heart_disease.pdf", height = 8, width = 15) 

plot1 <- boxmap("avrg_d_", hdisease.dat, mtitle = "", legtitle = "mortality rate", mult = 1.5) +
tm_layout(title = "Coronary Heart Disease", title.position = c("left","top"))
print(plot1)
dev.off()  

# explore data for diabetes-related deaths 
diabetes.dat <- filter(dat, cs_f_dt == "diabetes-related")
# box map for mortality rates 
pdf("box_map_diabetes_related.pdf", height = 8, width = 15) 
plot2 <- boxmap("avrg_d_", diabetes.dat, mtitle = "", legtitle = "mortality rate", mult = 1.5) + 
tm_layout(title = "Diabetes-Related", title.position = c("left","top"))
print(plot2)
dev.off()  

# explore data for stroke (cerebrovascular disease)
stroke.dat <- filter(dat, cs_f_dt == "stroke (cerebrovascular disease)")
# box map for mortality rates 
pdf("box_map_stroke.pdf", height = 8, width = 15) 
plot3 <- boxmap("avrg_d_", stroke.dat, mtitle = "", legtitle = "mortality rate", mult = 1.5) +
tm_layout(title = "Stroke (cerebrovascular disease)", title.position = c("left","top"))
print(plot3)
dev.off()  

# explore data for injury, unintentional
injury.dat <- filter(dat, cs_f_dt == "injury, unintentional")
# box map for mortality rates 
pdf("box_map_heart_injury.pdf", height = 8, width = 15) 
plot4 <- boxmap("avrg_d_", injury.dat, mtitle = "", legtitle = "mortality rate", mult = 1.5) +
tm_layout(title = "Injury, Unintentional", title.position = c("left","top"))
print(plot4)
dev.off()  

############################## 
# PLACE PLOTS ON THE SAME PANEL 
############################## 
pdf("mortality_rates_box_maps_panel_view.pdf", height = 8, width = 15) 
current.mode <- tmap_mode("plot")
tmap_arrange(plot1, plot2, plot3, plot4)
tmap_mode(current.mode)
dev.off()

pdf("socioeconomic_variables_box_maps_panel_view.pdf", height = 8, width = 15) 
current.mode <- tmap_mode("plot")
tmap_arrange(plot_1, plot_2, plot_3)
tmap_mode(current.mode)
dev.off()

############################## 
# SD MAP
############################## 
tm_shape(hdisease.dat) +
  tm_fill("avrg_d_", title = "", style = "sd",palette = "-RdBu")  +
  tm_borders() +
  tm_layout(title = "Standard Deviation Map", title.position = c("right","bottom"))

############################## 
# REGRESSION ANAYLIS 
############################## 
# fit regression model for each underlying cause of death 
m1 <- lm(avrg_d_ ~ hrdshp_ + dstnc__ , data = hdisease.dat)
summary(m1)
m2 <- lm(avrg_d_ ~ hrdshp_ + dstnc__ , data = diabetes.dat)
summary(m2)
m3 <- lm(avrg_d_ ~ hrdshp_ + dstnc__ , data = stroke.dat)
summary(m3)
m4 <- lm(avrg_d_ ~ hrdshp_ + dstnc__ , data = injury.dat)
summary(m4)

setwd("/Users/jesuscantu/Desktop/U_Chicago/Spatial_Data_Science_Project/Figures")

library(stargazer)
stargazer(m1, m2, m3, m4, type = "html",
          dep.var.labels = c("(1)", "(2)", "(3)", "(4)"),
          covariate.labels = c("Hardship Index", "Distance from Nearest Hospital (km)"),
          dep.var.caption = "Mortality Rates",
          out = "reg_models1.doc")

# fit regression model for each underlying cause of death 
m1 <- lm(avrg_d_ ~ pr_cpt_ + dstnc__ , data = hdisease.dat)
summary(m1)
m2 <- lm(avrg_d_ ~ pr_cpt_ + dstnc__ , data = diabetes.dat)
summary(m2)
m3 <- lm(avrg_d_ ~ pr_cpt_ + dstnc__ , data = stroke.dat)
summary(m3)
m4 <- lm(avrg_d_ ~ pr_cpt_ + dstnc__ , data = injury.dat)
summary(m4)

stargazer(m1, m2, m3, m4, type = "html",
          dep.var.labels = c("(1)", "(2)", "(3)", "(4)"),
          covariate.labels = c("Per Capita Income", "Distance from Nearest Hospital (km)"),
          dep.var.caption = "Mortality Rates",
          out = "reg_models2.doc")



############################## 
# EXTRA TABLES
############################## 
table.dat <- stroke.dat[, c("commnty", "hrdshp_", "avrg_d_", "pr_cpt_")]

# sort data by per capita income (descending)
table.dat.st <- table.dat[order(-table.dat$hrdshp_),]
table.dat.st.sub <- table.dat.st[c(1:15), ] #select first 20 rows

