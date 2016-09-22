load_all()
load_all("~/code/owensData")
library(rgdal)
library(raster)
library(ggplot2)
library(dplyr)
library(reshape2)
rasterOptions(tolerance = 1)

# read in SFWCRFT area shapefile
sfwct_areas <- readOGR(path.expand("~/code/sfwcRft/data/DCM_SFWCT_2015_Bndys_070815"), 
               "DCM_SFWCT_2015_Bndys_070815")
sfwct_areas@data$index <- as.integer(paste0(substr(sfwct_areas@data$DCM, 2, 3), 
                                 as.character(sfwct_areas@data$TrgtWet)))

# build trimmed rasters in shape of areas for trimming SWIR images. 
ras_uniform <- raster("~/dropbox/data/swir/Formation_noadj/113015_form_noadj.tif")
ras_uniform[ , ] <- 1
# sfwct_ras <- ras_clip(ras_uniform, sfwct_areas)
sfwct_trgtwet_ras <- ras_clip(ras_uniform, sfwct_areas, fld="index")

# for which dates are SWIR images available?
# do not use May 9, 2015 (cloud cover) (June 8, 2015 will be used for May 2015 
# wetness estimate)
date_index <- c("041615", "060815", "062015", "113015", "120115", 
                "042616", "052716", "062416")

# get list of file names for SWIR images
a <- list.files(path="~/dropbox/data/swir/Formation_noadj/", 
                pattern=".tif")

wetness <- vector(mode="list", length=length(date_index))
names(wetness) <- date_index
for (k in date_index){
  print(k)
  fn <- a[substr(a, 1, 6)==k][1]
  ras_swir <- raster(paste0("~/dropbox/data/swir/Formation_noadj/", fn))
#  ras_uniform <- ras_swir
#  ras_uniform[ , ] <- 1
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

names(wetness)[names(wetness)=='060815'] <- '05xx15'
names(wetness) <- paste0(substr(names(wetness), 1, 2), 
                                 substring(names(wetness), 5))

# define months in dust season for which wetness is required for CE calculations
month_index <- c("0415", "0515", "0615", "1115", "1215", "0116",
                 "0216", "0316", "0416", "0516", "0616")
ce_wetness <- vector(mode="list", length=length(month_index))
names(ce_wetness) <- month_index

swir_wet_tbl <- data.frame(dca=wetness[[1]]$dca, 
                          trgtwet=wetness[[1]]$trgtwet)
for (i in names(wetness)){
  names(wetness[[i]])[3] <- paste0("swir.", i)
  swir_wet_tbl <- left_join(swir_wet_tbl, wetness[[i]], 
                           by=c("dca", "trgtwet"))
}

# pull transect wetness data to fill in missing SWIR months
query1 <- "SELECT dcm as dca, trgtwet, day, round AS wet
           FROM sfwct.wetness_summary 
           WHERE class='W'"
trans_summ <- query_owenslake(query1)
# remove April 20, 2016 T26 transects, use April 26 instead to coincide with SWIR 
# flight. Remove duplicated day in Jun for T10 65%
trans_summ <- filter(trans_summ, !(day=='2016-04-20' & dca=='T26'))
trans_summ <- filter(trans_summ, !(day=='2015-06-09' & dca=='T10' & trgtwet==65))
# add in 0% wetness in T26 0% area for all transect days (this is what was 
# measured, but since wetness transect has 0 length for these areas, it will not 
# show up in summary view
a <- unique(filter(trans_summ, dca=='T26')$day)
tmp <- data.frame(dca=rep('T26', length(a)), trgtwet=rep(0, length(a)), 
                  day=a, wet=rep(0, length(a)))
tmp1 <- data.frame(dca='T26', trgtwet=45, day='2016-03-08', wet=0) 
tran_record <- rbind(trans_summ, tmp, tmp1)
tran_record$period <- paste0(substr(tran_record$day, 6, 7), 
                             substr(tran_record$day, 3, 4))
# apply June 8 transect to may 2015 period
tran_record[tran_record$period=='0615', ]$period <- '0515'
trans_sub <- filter(tran_record, !(period %in% names(wetness)))
# apply January 28 transect wetness to both Jan 2016 and Feb 2016 period
tmp <- trans_sub %>% filter(period=='0116') %>% select(-period) %>%
  mutate(period='0216')
trans_sub <- rbind(trans_sub, tmp) %>% select(-day) %>%
  arrange(period, dca, trgtwet)
trans_sub$period <- paste0("tran.", trans_sub$period)

wet_record <- melt(swir_wet_tbl, id.vars=c("dca", "trgtwet"), 
                   variable.name="period", value.name="wet") %>%
              rbind(trans_sub) %>% rename(index=period) %>%
              mutate(method=substr(index, 1, 4), period=substr(index, 6, 9)) %>%
              select(-index)
wet_record$trgtwet <- as.integer(wet_record$trgtwet)
wet_record$period <- ordered(wet_record$period, 
                             levels=c('0415', '0515', '0615', '1115', '1215',
                                      '0116', '0216', '0316', '0416', '0516',
                                      '0616'))
full_record <- expand.grid(period=unique(wet_record$period), 
                           dca=unique(wet_record$dca), 
                           trgtwet=unique(wet_record$trgtwet)) %>%
               filter(!(dca=='T13' & trgtwet!=75), 
                      !(dca=='T29' & !(trgtwet %in% c(0, 45))))               
wet_record <- left_join(full_record, wet_record, 
                        by=c("dca", "trgtwet", "period")) %>%
              arrange(dca, period, trgtwet)
wet_record <- wet_record[!(duplicated(wet_record)), ]

save(wet_record, tran_record, file="./data/wetness.RData")








  
  

