###########
## SETUP ##
###########

#####################################
## Set working directory to input data
wd <- setwd("C:/Users/clittlef/Google Drive/3Other proj/2020.11 Second College Grant Carbon/SCG/data") 
data.dir <- "C:/Users/clittlef/Google Drive/3Other proj/2020.11 Second College Grant Carbon/SCG/data/"
out.dir <- "C:/Users/clittlef/Google Drive/3Other proj/2020.11 Second College Grant Carbon/SCG/output/"
code.dir <- "C:/Users/clittlef/Google Drive/3Other proj/2020.11 Second College Grant Carbon/SCG/"


#####################################
# Install packages if not already installed
required.packages <- c("plyr", "ggplot2", "gridExtra", "raster", "sf", "rgdal", "dplyr",
                       "tidyverse", "maptools", "rgeos", 
                       "partykit", "vcd", "maps", "mgcv", "tmap",
                       "MASS", "pROC", "ResourceSelection", "caret", "broom", "boot",
                       "dismo", "gbm", "usdm", "pscl", "randomForest", "pdp", "classInt", "plotmo",
                       "ggspatial", "lmtest",  "dynatopmodel", "spatialEco")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages)
rm(required.packages, new.packages)

# Libraries
# library(plyr)
library(ggplot2)
library(gridExtra)
library(raster)
# library(sp)
library(sf)
library(rgdal)
library(dplyr)
library(tidyverse)
library(maptools)
library(rgeos)
library(partykit)
library(vcd)
library(maps)
library(mgcv)
library(tmap)
library(MASS)
library(pROC)
library(ResourceSelection)
library(caret)
library(broom)
library(boot)
library(dismo)
library(gbm)
library(usdm)
library(pscl)
library(randomForest)
library(pdp)
library(classInt)
library(plotmo)
library(ggspatial)
library(lmtest)
library(dynatopmodel)
library(spatialEco)

# rm(GCtorture)

#####################################
# Turn off scientific notation
options(scipen=999) 


#####################################
# Grab date for saving files
currentDate <- Sys.Date()


#####################################
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(7, "Set1")
# palette <- brewer.pal(7, "Set1")

display.brewer.all(colorblindFriendly = TRUE)
display.brewer.pal(8, "Dark2")
display.brewer.pal(8, "RdYlBu")
palette <- brewer.pal(8, "Dark2")
palette <- brewer.pal(8, "Set2")
palette <- brewer.pal(8, "RdYlBu")




# Text extraction functions
left = function(text, num_char) {
  substr(text, 1, num_char)
}

mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}
```
