load_all("~/code/owensData")
load_all()
library(rgdal)
library(raster)
library(ggplot2)
library(dplyr)
rasterOptions(tolerance = 1)

sfwct_areas <- readOGR(path.expand("~/code/sfwcRft/data/DCM_SFWCT_2015_Bndys_070815"), 
               "DCM_SFWCT_2015_Bndys_070815")
sfwct_areas@data$index <- as.integer(paste0(substr(sfwct_areas@data$DCM, 2, 3), 
                                 as.character(sfwct_areas@data$TrgtWet)))
ras_uniform <- raster("~/dropbox/data/swir/Formation_noadj/113015_form_noadj.tif")
ras_uniform[ , ] <- 1
sfwct_ras <- ras_clip(ras_uniform, sfwct_areas)
sfwct_trgtwet_ras <- ras_clip(ras_uniform, sfwct_areas, fld="index")
date_index <- c("041615", "050915", "060815", "062015", "113015", "120115", 
                "042616", "052716", "062416")
a <- list.files(path="~/dropbox/data/swir/Formation_noadj/", 
                pattern=".tif")
wetness <- vector(mode="list", length=length(date_index))
names(wetness) <- date_index
for (k in date_index){
  print(k)
  fn <- a[substr(a, 1, 6)==k][1]
  ras_swir <- raster(paste0("~/dropbox/data/swir/Formation_noadj/", fn))
  ras_uniform <- ras_swir
  ras_uniform[ , ] <- 1
  zones <- vector(mode="list", length=length(sfwct_areas@data$index))
  names(zones) <- sfwct_areas@data$index
  for (i in names(zones)){
    print(paste0("Building raster: ", i))
    temp <- sfwct_trgtwet_ras
    temp[ , ] <- sapply(temp[ , ], function(x) ifelse(x==i, 1, NA))
    zones[[i]] <- temp
  }

  wetness[[k]] <- data.frame(dca=c(), trgtwet=c(), wetness=c())
  for (j in names(zones)){
    print(paste0("Calculating wetness: ", j))
    dca_txt <- paste0("T", substr(j, 1, 2))
    trgt_txt <- substring(j, 3)
    reflect_ras <- ras_swir * zones[[j]]
    wet_ras <- class_wet(reflect_ras)
    wet_value <- sum(wet_ras[ , ], na.rm=T)/sum(!is.na(wet_ras[ , ]))
    temp <- data.frame(dca=dca_txt, trgtwet=trgt_txt, 
                       wetness=round(wet_value, 2))
    wetness[[k]] <- rbind(wetness[[k]], temp)
  }
}

focused_wetness <- wetness[!(names(wetness) %in% c("050915", "062015"))]
names(focused_wetness) <- paste0(substr(names(focused_wetness), 1, 2), 
                                 substring(names(focused_wetness), 5))
month_index <- c("0415", "0515", "0615", "1015", "1115", "1215", "0116",
                 "0216", "0316", "0416", "0516", "0616")
ce_wetness <- vector(mode="list", length=length(month_index))
names(ce_wetness) <- month_index

swir_summ <- data.frame(dca=focused_wetness[[1]]$dca, 
                          trgtwet=focused_wetness[[1]]$trgtwet)
for (i in names(focused_wetness)){
  names(focused_wetness[[i]])[3] <- i
  swir_summ <- left_join(swir_summ, focused_wetness[[i]], 
                           by=c("dca", "trgtwet"))
}
# remove areas when not in operation
swir_summ[swir_summ$dca=='T26', 3:4] <- NA
swir_summ[swir_summ$dca=='T29', 3] <- NA

swir_summ$average <- apply(as.matrix(swir_summ[ , 3:9]), 1, 
                           function(x) round(mean(x, na.rm=T), 2))
for (j in names(ce_wetness)){
  if (is.null(swir_summ[[j]])){
    ce_wetness[[j]] <- select(swir_summ, dca, trgtwet, average)
  } else{
    ce_wetness[[j]] <- swir_summ[ , c('dca', 'trgtwet', j)]
  }
}
swir_summ$dca <- ordered(swir_summ$dca, levels=c("T26", "T10", "T29", "T13"))
swir_summ <- swir_summ %>% arrange(dca, trgtwet)

save(wetness, ce_wetness, swir_summ, file="./data/wetness.RData")
write.csv(swir_summ, file="~/Desktop/sfwct_swir_summary.csv")








  
  

