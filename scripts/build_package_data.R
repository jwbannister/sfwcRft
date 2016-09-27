rm(list=ls())
library(dplyr)

#' Get polygon data from shapefile
#' 
#' @param dsn String. Path to shapefile directory.
#' @param layer String. Name of shapefile.
#' @param proj_string String. CRS projection string in "proj4string" format.
#' @return Data frame with treatment area polygon data.
shape_data <- function(dsn, layer, proj_string){
  dsn <- path.expand(dsn)
  areas <- rgdal::readOGR(dsn=dsn, layer=layer, verbose=FALSE)
  areas <- sp::spTransform(areas, proj_string)
  dat <- areas@data 
  labpnts <- lapply(c(1:length(areas@polygons)), 
                    function(x) areas@polygons[[x]]@labpt)
  polypnts <- lapply(c(1:length(areas@polygons)), 
                     function(x) areas@polygons[x][[1]]@Polygons[[1]]@coords)
  area_data <- cbind(dat, I(labpnts), I(polypnts)) 
  colnames(area_data) <- tolower(colnames(area_data))
  area_data
}

#' Build data frame from multiple lists contained in a data frame.
#' 
#' @param df_in Data frame. 
#' @param list_ind Integer. Column index of lists to process.
#' @param id_ind Integer. Column index of object id to be associated with all 
#' elements of corresponding list.
#' @return Data frame.
lists2df <- function(list_in){
  df_out <- data.frame(x=numeric(), y=numeric(), objectid=integer())
  for (i in 1:length(list_in)){
    df1 <- data.frame(matrix(list_in[[i]], ncol=2))
    df1$objectid <- rep(i, nrow(df1))
    colnames(df1)[1:2] <- c("x", "y")
    df_out <- rbind(df_out, df1)
  }
  df_out
}

proj_string <- paste0("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs", 
                      "+ellps=GRS80 +towgs84=0,0,0")
shp_dsn <- "~/dropbox/gis/owens/SFWCT/DCM_SFWCT_2015_Bndys_070815"
shp_layer <- "DCM_SFWCT_2015_Bndys_070815"
sfwct_dcm_data <- shape_data(shp_dsn, shp_layer, proj_string)
sfwct_polys <- lists2df(sfwct_dcm_data$polypnts)
sfwct_labels <- lists2df(sfwct_dcm_data$labpnts)
sfwct_labels <- inner_join(sfwct_labels,
                       select(sfwct_dcm_data, objectid, dcm, treatment), 
                       by="objectid")
sfwct_labels$label <- sfwct_labels$treatment
save(sfwct_polys, sfwct_dcm_data, sfwct_labels, 
     proj_string, file="./data/package_data.RData")
