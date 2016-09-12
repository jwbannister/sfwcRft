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
a <- list.files(path="~/dropbox/data/swir/DRI_original/",  
                pattern=".tif")
dri_wetness <- vector(mode="list", length=length(date_index))
names(dri_wetness) <- date_index
for (k in date_index){
  print(k)
  fn <- a[substr(a, 1, 6)==k][1]
  ras_swir <- raster(paste0("~/dropbox/data/swir/DRI_original/", fn))
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

  dri_wetness[[k]] <- data.frame(dca=c(), trgtwet=c(), wetness=c())
  for (j in names(zones)){
    print(paste0("Calculating wetness: ", j))
    dca_txt <- paste0("T", substr(j, 1, 2))
    trgt_txt <- substring(j, 3)
    reflect_ras <- ras_swir * zones[[j]]
    wet_ras <- class_wet(reflect_ras)
    wet_value <- sum(wet_ras[ , ], na.rm=T)/sum(!is.na(wet_ras[ , ]))
    temp <- data.frame(dca=dca_txt, trgtwet=trgt_txt, 
                       wetness=round(wet_value, 2))
    dri_wetness[[k]] <- rbind(dri_wetness[[k]], temp)
  }
}

focused_dri <- dri_wetness[!(names(dri_wetness) %in% c("050915", "062015"))]
names(focused_dri) <- paste0(substr(names(focused_dri), 1, 2), 
                                 substring(names(focused_dri), 5))
month_index <- c("0415", "0515", "0615", "1015", "1115", "1215", "0116",
                 "0216", "0316", "0416", "0516", "0616")
ce_wetness <- vector(mode="list", length=length(month_index))
names(ce_wetness) <- month_index

dri_summ <- data.frame(dca=focused_dri[[1]]$dca, 
                          trgtwet=focused_dri[[1]]$trgtwet)
for (i in names(focused_dri)){
  names(focused_dri[[i]])[3] <- i
  dri_summ <- left_join(dri_summ, focused_dri[[i]], 
                           by=c("dca", "trgtwet"))
}
# remove areas when not in operation
dri_summ[dri_summ$dca=='T26', 3:5] <- NA
dri_summ[dri_summ$dca=='T29', 3] <- NA

dri_melt <- reshape2::melt(dri_summ, id.vars=c("dca", "trgtwet"), 
                           value.name="IA adjusted")
dri_melt$variable <- ordered(dri_melt$variable, 
                             levels=c("0415", "0615", "1115", "1215", "0416", 
                                      "0516", "0616"), 
                             labels=c("Apr15", "June15", "Nov15", "Dec15", 
                                      "Apr16", "May16", "June16"))
swir_melt <- reshape2::melt(swir_summ, id.vars=c("dca", "trgtwet"), 
                           value.name="Atmos. adjusted")
swir_melt$variable <- ordered(swir_melt$variable, 
                             levels=c("0415", "0615", "1115", "1215", "0416", 
                                      "0516", "0616"), 
                             labels=c("Apr15", "June15", "Nov15", "Dec15", 
                                      "Apr16", "May16", "June16"))
swir_comp <- left_join(dri_melt, swir_melt, 
                       by=c("dca", "trgtwet", "variable"))
names(swir_comp)[3] <- "period"
comp_melt <- reshape2::melt(swir_comp, id.vars=c("dca", "trgtwet", "period"))
comp_melt$value <- comp_melt$value * 100
comp_melt$trgtwet <- ordered(comp_melt$trgtwet, levels=c(0, 45, 55, 65, 75), 
                             labels=c("Target 0%", "Target 45%", "Target 55%",
                                      "Target 65%", "Target 75%"))
for (i in c("T10", "T26")){
  p_comp <- comp_melt %>% filter(dca==i) %>%
    ggplot(aes(x=period, y=value, group=variable)) +
    geom_path(aes(color=variable)) +
    ylab("Estimated Wetness (%)") + xlab("") +
    theme(legend.title=element_text("Method")) +
    facet_grid(trgtwet ~ ., scales="free_y") +
    ggtitle(i)
  png(filename=paste0("~/Desktop/", i, "_compare.png"), width=9, height=6, 
      units="in", res=300)
      
  print(p_comp)
  dev.off()
}
  

save(dri_wetness, file="./data/dri_wetness.RData")
