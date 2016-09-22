rm(list=ls())
load_all()
library(tidyverse)
library(rgdal)

inst_locs <- 
  readOGR(path.expand("~/dropbox/gis/owens/SFWCT/SFWCT_final_locations"), 
                      "SFWCT_final_locations")@data
xtabs(~ desc + DCA, data=inst_locs)
