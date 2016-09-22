
#' Plot wind speeds prior to collection for SFWCT area.
#' 
#' @import dplyr
#' @import ggplot2
#' @param col_id Numeric. Collection id number. Wind speeds will be plotted from 
#' the end of the previous collection to the beginning of this collection.
#' @param area String. SFWCT area for which to plot wind speeds.
#' @param collections_df. Data frame. All sand collections made in SFWCT areas, 
#' with *collection.id, dcm, start, end* columns.
#' @return ggplot2 object. Plot of 5 minute 10m wind speeds over time between 
#' collections.
precollection_wind <- function(area,  
                               begin=start.date,
                               ending=end.date){
  wind.start <- begin
  wind.end <- ending
  met.index <- list("T10"="1451", "T13"="1452", "T26"="1551", "T29"="1552")
  query1 <- paste0(
                   "SELECT data.datetime, data.windspeed_10m, data.voltage,
                   inst.deployment, inst.description, data.winddirection_10m  
                   FROM mets.deployment_data data 
                   JOIN instruments.deployments inst 
                   ON inst.deployment_id=data.deployment_id 
                   WHERE inst.deployment='", met.index[area], 
                   "' AND data.datetime > '", wind.start,
                   "' AND data.datetime < '", wind.end, "'")
  df2 <- query_owenslake(query1)
  df2 <- filter(df2, windspeed_10m<500)
  p1 <- ggplot(df2, aes(x=datetime, y=windspeed_10m)) +
    geom_path() + 
    ggtitle(area) +
    ylab("10m Wind Speed (m/s)") + xlab("")
  p1
}

#' Plot SFWCT area with CSC masses for collection
#' 
#' @import dplyr
#' @import ggplot2
#' @param background GGplot object. Background image with treatment borders and 
#' labels.
#' @param sand_df Data frame of sand mass data.
#' @param area. String. Area to be plotted.
#' @return ggplot2 object. Plot of SFWCT area with sand catch masses and 
#' countour lines.
plot_csc_masses <- function(background, sand_df, area_txt,
                            period_txt){
  if (!is.na(area_txt)) catches <- sand_df %>% filter(dca==area_txt)
  if (!is.na(period_txt)) catches <- catches %>% filter(period==period_txt)
  if (nrow(catches)==0){
    return(ggplot(catches, aes(x=csc, y=sand.mass)) + geom_blank())
  } else{
    mass.range <- range(catches$sand.mass)[2] - range(catches$sand.mass)[1]
    df1 <- akima::interp(catches$x, catches$y, catches$sand.mass)
    df2 <- reshape2::melt(df1$z, na.rm=TRUE)
    names(df2) <- c("x.ind", "y.ind", "sand.mass")
    df2$x <- df1$x[df2$x.ind]
    df2$y <- df1$y[df2$y.ind]
    plot.title <- paste0(area_txt, " Sand Mass Contours (", period_txt, ")")
    leg.pos <- list("T26"=c(0.15, 0.2),
                    "T10"=c(0.85, 0.2),
                    "T13"=c(0.15, 0.8),
                    "T29"=c(0.15, 0.2))
    p1 <- background +
      geom_label(data=catches, mapping=aes(x=x, y=y, label=csc)) +
      geom_point(data=catches, mapping=aes(x=x, y=y, color=sand.mass, 
                                           shape="CSC Site")) +
coord_fixed() +
ggtitle(plot.title) +
stat_contour(data=df2, binwidth=mass.range/9,
             mapping=aes(x=x, y=y, z=sand.mass, 
                         color=..level..)) +
scale_colour_gradientn("Sand Mass (g)", limits=c(0, 200),  
                       colours=c("darkgreen", "yellow", "red"))+
labs(shape=NULL) +
theme(axis.ticks.x=element_blank(),
      axis.text.x=element_blank(),
      axis.title.x=element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.y=element_blank(),
      axis.title.y=element_blank(),
      legend.position=leg.pos[[area_txt]],
      legend.background=element_rect(color="black"),
      legend.box.just="left", 
      plot.title=element_text(size=12))
p1
  }
}  

#' Plot DCM areas with background
#' 
#' @import dplyr
#' @import ggplot2
#' @param areas String.
#' @param polys_df Data frame. Points defining polygons are DCM areas. 
#' @param lables_df Data frame. Area labels and position coordinates.
#' @param external_points Data frame. *x* and *y* coordinates of points external 
#' to polygons to be included in plot range. 
plot_dcm_background <- function(areas, polys_df, labels_df, 
                                external_points=NULL){
  polys <- polys_df %>%
    inner_join(select(labels_df, objectid, dcm), by="objectid") %>%
    filter(dcm %in% areas)
  labels <- labels_df %>% filter(dcm %in% areas)
  plot.range <- get_plot_range(polys, external_points)
  map <- raster::stack("~/dropbox/data/dustReport/data-raw/owens_background.tif")
  ext <- sp::SpatialPointsDataFrame(coords=cbind(x=plot.range$x, y=plot.range$y), 
                                data=data.frame(id=1:2), 
                                proj4string=raster::crs(map))
  map_sub <- raster::crop(map, raster::extent(ext))
  map_sub <- raster::aggregate(map_sub, 4)
  map_df <- raster::as.data.frame(map_sub, xy=T)
  map_df <- data.frame(x=map_df$x, y=map_df$y, r=map_df$owens_background.1, 
                       g=map_df$owens_background.2, b=map_df$owens_background.3)
  p1 <- 
  ggplot(data=map_df) + 
  coord_fixed() + 
  theme_bw() +
  geom_tile(aes(x=x, y=y, fill=rgb(r,g,b, maxColorValue = 255)), alpha=0.75) + 
  geom_path(data=polys, mapping=aes(x=x, y=y, group=objectid), color="black") +
  geom_text(data=labels, aes(x=x, y=y, label=label), color="black") +
  scale_fill_identity() + 
  scale_x_continuous(breaks=range(map_df$x)*c(1.01, 0.99), 
                     labels=range(map_df$x), expand = c(0,0)) +
  scale_y_continuous(breaks=range(map_df$y)*c(0.99, 1.01), 
                     labels=range(map_df$y), expand = c(0,0)) +
  theme(panel.grid=element_blank())
p1
}


#' Get coordinate ranges for square plot around DCM areas.
#' 
#' @param polys. Data frame. Points defining polygons of interest.
#' @param external_points Data frame. *x* and *y* coordinates of points external 
#' to polygons to be included in plot range. 
#' @return A list with the x and y ranges for a sqaure plot around the areas of 
#' interest.
get_plot_range <- function(polys, external_points=NULL){
  p.temp <- ggplot(polys, aes(x=x, y=y)) + geom_path() +
    geom_point(data=external_points)
  info <- ggplot_build(p.temp)
  plot_xrange <- info[[2]]$ranges[[1]]$x.range
  plot_yrange <- info[[2]]$ranges[[1]]$y.range
  maxspan <- max(c(plot_xrange[2] - plot_xrange[1], 
                   plot_yrange[2] - plot_yrange[1]))
  midpoint <- c(mean(plot_xrange), mean(plot_yrange))
  xrange <- c(midpoint[1] - (maxspan/2), 
              midpoint[1] + (maxspan/2))
  yrange <- c(midpoint[2] - (maxspan/2), 
              midpoint[2] + (maxspan/2))
  plot.range <- list(x=xrange, y=yrange)
  plot.range
}
 
