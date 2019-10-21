# Title: Val_Step1_cdecLocate
# Purpose: Create cdec location files, this uses the csv provided with lat long information
# on the cdec website and filters it for stations that collect turbidity 
# and crops it within the s5 boundary
# Status: #~! some stations apparently have both NTU and FNU stations need to change
# the string extracting file
# Author: Christiana Ade
# Date: 8/9/2019
# Modified: 9/22/2019 (just notes)
####################################################################################
## require packages
require(raster)
require(rgdal)
require(stringr)
require(tictoc)
require(tidyverse)
require(rgeos)
require(spdplyr)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START USER DEFINED VARIABLES       #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following variables should be defined by the user each time.
#### INPUT FILES ####
#1) Cdec stations csv downloaded from cdec
# want the sensors column to be read in as a character
cdecStat <- read_csv("./Data/Ancil/Cdec_Stations.csv", 
                     col_types = cols(Sensors = col_character()))
#2) the s5t5 bounding box
s5box <- readOGR("./Data/Vector/s5t5_swath.shp")

#3) sensor ids for turbidity
id <- str_c("27,","221,",sep = "|")

#### OUTPUT FILES ####
outDir <- "./Data/Vector"
outShp <- "cdec_turbidity_stations_s5t5"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START SCRIPT                      ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Only select turbidity stations from cdec ####
cdecTurb <- cdecStat %>% 
  # from the sensors column  match either 27 or 221. #! this could have problems bec is not exact match
  filter(str_detect(Sensors,id)) %>%
  # add column that has which turbidity station a sensor has
  mutate(turbNum = str_extract(Sensors, "(27)|(221)"),
  # add a column for the turbidity units 27 = NTU, 221 = FNu
         turbUnits = ifelse(turbNum == 27, "NTU","FNU"))

#### change to spatial points dataframe and project ####
cdec.spdf <- SpatialPointsDataFrame(cdecTurb[, c("Longitude", "Latitude")],data = cdecTurb[1:ncol(cdecTurb)])
# need to assign the cordinates as lat and long 
proj4string(cdec.spdf) = CRS("+proj=longlat +datum=WGS84")
# transform to state projection
ct <- spTransform(cdec.spdf,  "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

#### intersect with s5t5 box ####
intRas <- raster::intersect(ct,s5box)
# remove unesc. columns added because of intersect
fShp <- intRas %>%
  select(-c(ID,GRIDCODE,Area))

# write data
writeOGR(fShp, outDir, outShp , driver = "ESRI Shapefile", overwrite_layer = T)
