# Title: Val_02_cdecQuery.R (Old:Val_Step2_cdecQuery.R)
# Purpose: Query Cdec for the turbidity values closest to s5t5 dates
# Status:
# Author: Christiana Ade
# Date: 8/9/2019
# Modified: 9/21/2019 - added an option for querying hourly stations in the 
# 10/21/2019 re-ran with new shapefile oct21 - did not actually end up changing the name
# extractCdec function
# **Requires** 
# 1) List of the Satellite acquisition times. 
####################################################################################
## require packages
require(raster)
require(CDECRetrieve)
require(lubridate)
require(data.table)
require(rlist)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START USER DEFINED VARIABLES       #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following variables should be defined by the user each time.
#### INPUT FILES ####
# 1) cdec stations location shape 
cShp <- readOGR("./Data/Vector/cdec_turbidity_stations_s5t5.shp")
# 2) S5t5 acquistion times
aqTimes <- read_csv("./Data/Ancil/S5_aquTime.csv")

#### OUTPUT FILES ####
# 1) output directory for final csv file
outDir <- "./Output/CSV"
# 2) output file for the matching times with cdec_query
outFN <- "cdec_s5Time_match_oct21.csv" 


#### USER DEFINED FUNCTIONS ####
# 1) extractCdec - written below, it includes hardcoded variables #~!
# 2) matching two date times

matchTimes <- function(shortData, longData) {
  # Identify a date time from lubridate
  is.POSIXct <- function(x) inherits(x, "POSIXct")
  if (is.null(longData) == FALSE){
    # find the datetime column in the short dataset
    # will have to add a useful error message about which column to select then
    dtS <- which(as.data.frame(sapply(shortData, is.POSIXct)) == T)
    dtL <- which(as.data.frame(sapply(longData, is.POSIXct)) == T)
    # create data tables from the two datasets
    srtDT <- data.table(shortData,key=names(shortData)[dtS])
    lnDT <-  data.table(longData,key=names(longData)[dtL])
    # add a second column so that the time data from the long dataset is retained
    lnDT$longDat <- longData[dtL]
    # filtered dataset
    new<-lnDT[srtDT,roll='nearest']
    return(new)
  } else {
    return("noData")
  }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 10/21/2019
# re ran on this date with new file name
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 9/21/2019 was originally run on 8/9/2019, but 
# a new line was added to the extractCdec functions to query stations 
# that had hourly time stamps, but no event time stamps. This 
# includes stations BKS,CLC,KA0
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START SCRIPT                      ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# edit aqTimes file
aqTimes <- aqTimes %>%
  # change times to date class
  mutate(SPOT_Acquisition_Dates = mdy(SPOT_Acquisition_Dates)) %>%
  # combine date and time columns
  mutate(satDT = ymd_hms(paste(SPOT_Acquisition_Dates,Acquistion_Times )))
# start time
strt <- min(aqTimes$SPOT_Acquisition_Dates)
# end time
endT <- max(aqTimes$SPOT_Acquisition_Dates)

#~! This function includes hardcoded variables
extractCdec <- function(stationName, df){
  # separate by stationName
  myRow <- df %>%
    filter(STA == stationName)
  # Enter into the CDECRetrieve::cdec_query function
  # return NULL for any station that cannot be queried 
  cdecDat <- tryCatch(cdec_query(myRow$STA, sensor = myRow$turbNum, dur_code = 'E', 
                      start_date= strt, end_date = endT), error=function(e) NULL)
  if (is.null(cdecDat)){
    cdecDat <- tryCatch(cdec_query(myRow$STA, sensor = myRow$turbNum, dur_code = 'H', 
                                   start_date= strt, end_date = endT), error=function(e) NULL)
  }

  return(cdecDat)
}

# list of station names 
stN <- unique(cShp$STA)
# apply extractCdec function to all stations 
ec <- lapply(stN, extractCdec, df = cShp@data)

# applied matched times function to match 
# the closest station observation to satellite overpass
mt <- lapply(ec,matchTimes, shortData = aqTimes)
# find which entries had no data
bd <- which(mt== "noData")
# bind all rows together and remove those with no data
z2 <- do.call(bind_rows,mt[-c(bd)])

# add the data back in from the shpfile
z3 <- z2 %>%
  # remove certain columns
  select(-c(SPOT_Acquisition_Dates,parameter_cd,Acquistion_Times)) %>%
  # rename
  rename(turb_val = parameter_value,
         STA = location_id,
         stationTime = longDat) %>% 
  # rejoin shapefile information
  left_join(cShp@data)

# write out csv
write_csv(z3, paste0(outDir,"/",outFN))
