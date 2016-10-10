rm(list=ls())
load_all()
load_all("~/code/owensData")
library(rgdal)
library(raster)
library(ggplot2)
library(dplyr)
library(reshape2)
rasterOptions(tolerance = 1)

# read in SFWCRFT area shapefile
sfwct_areas <- readOGR(path.expand("~/dropbox/gis/owens/SFWCT/DCM_SFWCT_2015_Bndys_070815"), 
               "DCM_SFWCT_2015_Bndys_070815")
sfwct_areas@data$index <- as.integer(paste0(substr(sfwct_areas@data$DCM, 2, 3), 
                                 as.character(sfwct_areas@data$TrgtWet)))

# build trimmed rasters in shape of areas for trimming SWIR images. 
ras_uniform <- raster("~/dropbox/data/swir/LS_adjusted/042616_LSadj.tif")
ras_uniform[ , ] <- 1
# sfwct_ras <- ras_clip(ras_uniform, sfwct_areas)
sfwct_trgtwet_ras <- ras_clip(ras_uniform, sfwct_areas, fld="index")

index <- c("1st", "2nd", "3rd")
wetness <- vector(mode="list", length=length(index))
names(wetness) <- index
for (k in index){
  print(k)
  fn <- paste0("0624_", k, ".tif")
  ras_swir <- raster(paste0("~/dropbox/data/swir/LS_adjusted/drying/", fn))
  zones <- vector(mode="list", length=length(sfwct_areas@data$index))
  names(zones) <- sfwct_areas@data$index
  for (i in names(zones)){
    print(paste0("Building raster: ", i))
    temp <- sfwct_trgtwet_ras
    temp[ , ] <- sapply(temp[ , ], function(x) ifelse(x==i, 1, NA))
    zones[[i]] <- temp
  }
  wetness[[k]] <- data.frame(dca=c(), trgtwet=c(), swir.wet=c())
  for (j in names(zones)){
    print(paste0("Calculating wetness: ", j))
    dca_txt <- paste0("T", substr(j, 1, 2))
    trgt_txt <- substring(j, 3)
    reflect_ras <- ras_swir * zones[[j]]
    wet_ras <- class_wet(reflect_ras)
    wet_value <- sum(wet_ras[ , ], na.rm=T)/sum(!is.na(wet_ras[ , ]))
    temp <- data.frame(dca=dca_txt, trgtwet=trgt_txt, 
                       swir.wet=round(wet_value, 2))
    wetness[[k]] <- rbind(wetness[[k]], temp)
  }
}

wetness[[1]]$flight <- 1
wetness[[2]]$flight <- 2
wetness[[3]]$flight <- 3

dry_df <- rbind(wetness[[1]], wetness[[2]], wetness[[3]])
dry_df$swir.wet <- dry_df$swir.wet * 100

p1 <- dry_df %>% filter(trgtwet!=0) %>%
    ggplot(aes(x=flight, y=swir.wet, group=trgtwet)) +
    geom_path(aes(color=trgtwet)) +
    facet_grid(dca ~ ., scales="free_y") +
    scale_x_continuous(name="Approx. Hours Drying", breaks=c(1, 2, 3), 
                       labels=c("0", "1.5", "4.5")) +
    scale_y_continuous(name="SWIR Wetness", breaks=seq(0, 100, 10), 
                       minor_breaks=seq(5, 95, 10)) 

