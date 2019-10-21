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
require(CDECRetrieve)
require(lubridate)
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
# 1) out directory for vector files
outDir <- "./Data/Vector"
# 2) out directory for final cdec shapefile
outShp <- "cdec_turbidity_stations_s5t5_oct21"

#### USER DEFINED FUNCTIONS ####
durFil <- function(df) {
  if(nrow(df)> 1){
    if("event" %in% df$duration){
      df_fil <- df %>% filter(duration == "event")
    } else {
      df_fil <- df %>% filter(duration == "hourly")
    } 
  } else {
    return(df)
  }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 10/20
# previous versions did not have ", after the numbers in id
# a new line was added to remove stations that were not 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START SCRIPT                      ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Only select turbidity stations from cdec ####
cdecTurb <- cdecStat %>% 
  # from the sensors column  match either 27 or 221. #! this could have problems bec is not exact match
  filter(str_detect(Sensors,id)) #%>%
  # # add column that has which turbidity station a sensor has
  # mutate(turbNum = str_extract(Sensors, "(27)|(221)"),
  # # add a column for the turbidity units 27 = NTU, 221 = FNu
  #        turbUnits = ifelse(turbNum == 27, "NTU","FNU"))

#### Identify Start Date of the Turbidity Sensor ####
# Station Ids
staId <- cdecTurb$STA
# query cdec datasets for information
statInfo <- sapply(staId,cdec_datasets, keyword = "turbidity", USE.NAMES = T,simplify = F)
# remove repeat entries for duration hourly and daily
statInfo <- lapply(statInfo,durFil)
# add a STA column and bind all rows
statInfo <- mapply(cbind, statInfo, "STA"=staId, SIMPLIFY=F) %>% bind_rows() %>% filter(!duration == "daily")

#### Bind Cdec Station Information tgoether and change column names ####
cdecTurb <- cdecTurb %>% 
  # join station information that includes the sensor start and end date, ntu/fnu. and number of sensor
  left_join(statInfo) %>%
  # rename columns
  rename("turbNum" = sensor_number, turbUnits = sensor_units, sensStart = start, sensEnd = end) %>%
  dplyr::select(-sensor_name) 
# anything that has and end date of 10/21/2019 makes the sensors is still collecting data
  #mutate(end = ifelse(end >= Sys.Date()))

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
