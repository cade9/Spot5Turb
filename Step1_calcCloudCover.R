# Title: calcCloudCover
# Status: Completed 9/9/2019
# Author: Christiana Ade
# Date: 9/9/2019
# Modified: 
# Purpose: This script takes the _NUA.tifs from each of the spot5take5 files and 
# uses them to calculate the % cloudcover intersecting the delta
# It does use a combination of Arcmap and R and should be turned into a notebook
####################################################################################
## require packages
require(raster)
require(rgdal)
require(lubridate)
require(tidyverse)
erequire(stringr)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START USER DEFINED VARIABLES       #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following variables should be defined by the user each time.
#### INPUT FILES ####
#1) Directory of S5T5 masks
s5Dir <- "Z:\\Data\\Raster\\Spot5Take5"
# 2) Cropped mask stacks from ArcMap 
cropFN <- "./Data/Raster/S5_CC_Masks_Eq1/Stack_NUA_clip_shruti_waterways_merge.tif"
#2) box for cropping 
s5Box <- readOGR("./Data/Vector/s5t5_swath.shp")
box <- readOGR("./Data/Vector/shruti_waterways_merge.shp")
#3) 
#### OUTPUT FILES ####
# 1) Out name for stacked masks
mskOut <- "Z:\\Cade\\Spot5Turb\\Data\\Raster\\S5_CC_Masks_Eq1"
# 2) outArea all
outArea <- "Z:\\Cade\\Spot5Turb\\Data\\Ancil\\S5_CC_cloudCount.csv"
#### USER DEFINED FUNCTIONS ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### This script was completed on 9/9
# It determined that dates 20150421,20150521,20150526, 20150720,20150804
# should be removed. And the 20150511 (9% CC), 20150516 (5.8 CC), and
# 20150620 (13% CC) should be considered for removal
# but I will process all of them and likely delete 20150620 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START Processing                      ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##################### PART 1: Export stack of masks ###################################
# list all the mask files 
mskL <- list.files(s5Dir, recursive = T, full.names = T, pattern = "NUA.TIF")
# create a stak of masks
mskStack <- stack(mskL)
# write raster
writeRaster(mskStack, "Z:\\Cade\\Spot5Turb\\Data\\Raster\\S5_CC_Masks_Eq1\\Stack_NUA.TIF",
            datatype = "INT2S", format = "GTiff")

##################### PART 2: Crop the mask to the shrutiMerge in ARCMAP #####################################
# Used Arcmap 
# Tool: Extract by Mask (Spatial Analyst)
# Input Raster: Z:\Cade\Spot5Turb\Data\Raster\S5_CC_Masks_Eq1\Stack_NUA.tif
# Input Raster or mask data: Z:\Cade\Spot5Turb\Data\Vector\shruti_waterways_merge.shp
# Output Raster: Z:\Cade\Spot5Turb\Data\Raster\S5_CC_Masks_Eq1\Stack_NUA_clip_shruti_waterways_merge.tif

##################### PART 3 : Determine area in Shruti box #####################################
# r <- raster(mskL[1])
# r[r >= 0] <- 1
# writeRaster(r,"Z:\\Cade\\Spot5Turb\\Data\\Raster\\S5_CC_Masks_Eq1\\areaShrutiRaster.tif",
#       datatype = "INT2S", format = "GTiff")
# Also extracted by mask in R
# the count is 5321426

##################### PART 4: Loop through area calculation ###################################
# read in mask stack
s5CropRas <- stack(cropFN)
# change raster names 
rNames <- paste0("ras_",str_extract(mskL,"(2015)\\d{4}"))
names(s5CropRas) <- rNames

## Define blank area all
areaAll <- NULL

for (i in 1:nlayers(s5CropRas)){
  #### Raster Conversions #### 
  # read in raster 
  ras <- s5CropRas[[i]] #i
  # set the mask NA value to 0
  NAvalue(ras) <- 0
  # convert anything above one 1 one because it is a cloud
  ras[ras > 0 ] <- 1 
  # raster date
  rDate <- str_extract(names(ras),("(2015)\\d{4}"))
  
  #### Area Calculations #### 
  # determine resolution
  resR <- res(ras)
  # determine frequency
  layerFreq <- freq(ras, useNA = "no")

  # area information
  areaTibble <- tibble( count = layerFreq[,'count'],
                     area_km2 = layerFreq[,'count'] * prod(resR) * 1e-06,
                     classVal = layerFreq[,'value'],
                     rasDate = rDate)
  areaAll  <- rbind(areaAll,areaTibble)
  

  ## remove temps
  rm(ras)
  removeTmpFiles(h = 0)
}

##################### PART 5: Edit extracted area ###################################
areaAllEdit <- areaAll %>%
  # NA omit any rows
  na.omit() %>%
  # add the area of the water ways determined in step 3
  mutate(waterwaysArea = 5321426,
    # compute the percent cloud cover
    percentCC = signif(count/waterwaysArea, digits = 2) * 100)

## write out csv file
#### Written out on 9/9/2019
write_csv(areaAllEdit,outArea)

##################### PART 6: Determine Dates to remove #####################################
areaAllEdit %>% filter(percentCC > 10.0) %>% select(rasDate)


