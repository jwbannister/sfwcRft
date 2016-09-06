
ras_clip <- function(ras, shp, fld=1){
          a1_crop <- crop(ras, shp)
          step1 <- rasterize(shp, a1_crop, field=fld)
          a1_crop * step1
}

class_wet <- function(ras, teeter=2080){
  ras[ , ] <- sapply(ras[ , ], function(x) ifelse(x<2080, 1, 0))
  ras
}
