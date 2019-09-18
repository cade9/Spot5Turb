# Title: Calculate dekker algorithm on the surface reflectance data 
# Status:
# Author: Christiana Ade
# Date: 8/9/2019
# Modified: 9/9/2019
# **Requires** 
# 1)  
####################################################################################
## require packages
require(raster)
require(rgdal)
require(stringr)
require(tictoc)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START USER DEFINED VARIABLES       #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following variables should be defined by the user each time.
#### INPUT FILES ####
# 1) S5t5 ref
s5L <- list.files("Z:\\Data\\Raster\\Spot5Take5\\S5T5_rep_ref_masked", pattern = ".tif$", full.names = T)
#### OUTPUT FILES ####
# 1) outDirectory for dekker
outDir <- "./Output/Raster/S5T5_dekker_ref"
# 2) append name 
outApp <- "dek_ref"
#### USER DEFINED FUNCTIONS ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Ran 9/9/2019
# Code was finished running afternoon of the date above
# important to note that the scale factor has not been adjusted
# and before computing any statistics, the resulting values should be 
# divided by 1000
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START SCRIPT                      ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dates want to remove based on cloud cover values being too high
dRemove <- str_c("0421","0521","0526","0720","0804", sep = "|")
s5L <- s5L[!str_detect(s5L,dRemove)] # should only be 20 entries

#### Apply Dekker Algorithm to data ####
for (file in s5L){
  tic(paste('processing', file))
  # read in rrs file
  ref <- stack(file)
  
  # dekker alg
  # tic("dek1")
  # dek <- (rrs[[1]] + rrs[[2]])/2
  # toc() 25.99
  #tic("dek2")
  dek <- overlay(ref[[1]],ref[[2]], fun=function(x,y){return((x+y)/2)})
  #toc() #18.75
  # save file
  bn <- str_extract(file, "(?=[^\\\\|/]*$)(.)+(?=\\.)")
  #fileName
  fn <- paste0(outDir,"/",bn,"_",outApp,".tif")
  # write raster
  writeRaster(dek, fn, datatype = "FLT8S", format = "GTiff", overwrite = T)
  toc()
  rm(ref)
  rm(dek)
  removeTmpFiles(h=0)
}
