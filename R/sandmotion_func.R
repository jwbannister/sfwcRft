
pull_5min_flux <- function(start.date, end.date, descrip){
  query1 <- paste0("SELECT flux.datetime, flux.sensit, flux.sand_flux, 
                   flux.windspeed_10m, flux.winddirection_10m, flux.invalid,
                   idep.deployment AS csc, idep.easting_utm AS x, 
                   idep.northing_utm AS y, ia.area AS dca
                   FROM sandcatches.sandflux_5min flux
                   JOIN instruments.deployments idep
                   ON flux.csc_deployment_id=idep.deployment_id
                   JOIN instruments.areas ia
                   ON idep.area_id=ia.area_id
                   WHERE datetime::date 
                   BETWEEN '", start.date, "'::date AND '", end.date, "'::date
                   AND ia.description='", descrip, "'")
  flux_data <- query_owenslake(query1)
  flux_data <- filter(flux_data, !(sand_flux<0))
  flux_data <- flux_data[!flux_data$invalid, ]
  csc_locs <- select(flux_data, csc, x, y)[!duplicated(flux_data$csc), ]
  df1 <- clean_flux_sfwct(flux_data) 
  flux_df <- mutate(df1, area=paste0(dca, "_", treatment), 
                    month=month(datetime), year=year(datetime), 
                    period=paste0(formatC(month, width=2, format="d", flag="0"), 
                                  substring(year, 3)))
}

descrip <- "Shallow Flood Wetness Cover Refinement Field Test"
pull_csc_locs <- function(descrip){
  query1 <- paste0("SELECT DISTINCT
                   idep.deployment AS csc, idep.easting_utm AS x, 
                   idep.northing_utm AS y
                   FROM instruments.deployments idep
                   JOIN instruments.areas ia
                   ON idep.area_id=ia.area_id
                   WHERE ia.description='", descrip, "'")
  csc_data <- query_owenslake(query1)
  csc_locs <- csc_data[substr(csc_data$csc, 2, 3)!="Cam", ]
  csc_locs
}

clean_flux_sfwct <- function(df_in){
  df_out <- df_in 
  df_out$dca <- gsub("-", "", df_out$dca)
  df_out$dca <- substr(df_out$dca, 1, 3)
  cscs <- df_out[!duplicated(df_out$csc), ]
  cscs <- dplyr::select(cscs, csc, x, y)
  cscs <- assign_dcm(cscs, sfwct_polys)
  cscs <- inner_join(cscs, select(sfwct_dcm_data, objectid, treatment), 
                     by="objectid")
  cscs <- select(cscs, csc, treatment)
  df_out <- inner_join(df_out, cscs, by="csc") %>% select(-x, -y, -invalid)
  df_out
}

summarize_flux_sfwct <- function(df_in, wet_df){
  wet_df$dryness <- 1 - wet_df$wet
  df_in$trgtwet <- as.integer(gsub("%", "", df_in$treatment))
  treat_sum <- df_in %>% group_by(dca, trgtwet, day, period) %>% 
    summarize(avg.flux=mean(sand.flux)) %>% ungroup() 
  treat_sum <- left_join(treat_sum, wet_df, 
                         by=c("dca", "trgtwet", "period"))
  control_sum <- filter(treat_sum, trgtwet==0) %>% 
    select(-trgtwet, -wet) %>% rename(control.flux=avg.flux, 
                                          control.dry=dryness)
  control_sum <- control_sum[!duplicated(control_sum), ]
  treat_sum <- left_join(treat_sum, control_sum, by=c("dca", "day")) %>%
    mutate(control.eff=1-((avg.flux*dryness)/(control.flux*control.dry)))
  treat_sum$control.eff <- round(treat_sum$control.eff, 2) * 100
  treat_sum[treat_sum$trgtwet==0, ]$control.eff <- NA
  treat_sum <- filter(treat_sum, trgtwet!=0) %>% 
    arrange(dca, day, trgtwet)
  treat_sum
}

#' Summarize SFWCT sand results for control efficiency
#' 
#' @import dplyr
#' @param df_in Data frame of sand mass data.
#' @return Data frame of sumamrized results.
summarize_ce <- function(df_in){
  control <- filter(df_in, treatment=="t_0") %>%
    select(area, day, control.sand=sand)
  control_sum <- df_in %>% 
    inner_join(control, by=c("area", "day")) %>%
    mutate(control.eff=1-(sand/control.sand))
  control_sum$control.eff <- round(control_sum$control.eff, 2)
  control_sum[control_sum$treatment=="t_0", ]$control.eff <- NA
  control_sum
}

rank_flux_cells <- function(df_in){
  maxes <- df_in %>% group_by(area) %>%
    summarize(t_45=max(t_45, na.rm=TRUE), t_55=max(t_55, na.rm=TRUE), 
              t_65=max(t_65, na.rm=TRUE), t_75=max(t_75, na.rm=TRUE))
  mins <- df_in %>% group_by(area) %>%
    summarize(t_45=min(t_45, na.rm=TRUE), t_55=min(t_55, na.rm=TRUE), 
              t_65=min(t_65, na.rm=TRUE), t_75=min(t_75, na.rm=TRUE))
  maxed <- vector(mode="list", length=4)
  names(maxed) <- colnames(maxes)[2:5]
  minned <- maxed
  clr <- maxed
  colorClass <- function(x){
    a <- 'white'
    if (x==1) a <- 'red'
    if (x==2 | x==3) a <- 'green'
    if (is.na(x)) a <- 'white'
    a
  }
  for (j in names(maxed)){
    for (i in maxes$area){
      d <- filter(df_in, area==i)[ , j]==max(filter(maxes, area==i)[ , j])
      maxed[[j]] <- c(maxed[[j]], d)
    }
    maxed[[j]] <- sapply(maxed[[j]], function(x) x*2)
  }
  for (j in names(minned)){
    for (i in maxes$area){
      d <- filter(df_in, area==i)[ , j]==min(filter(mins, area==i)[ , j])
      minned[[j]] <- c(minned[[j]], d)
    }
    minned[[j]] <- sapply(minned[[j]], function(x) x*1)
  }
  for (i in 1:length(clr)){
    clr[[i]] <- maxed[[i]] + minned[[i]]
    clr[[i]][is.na(clr[[i]])] <- 0
    clr[[i]][clr[[i]]==3] <- 2
  }
  clr
}

#' Summarize SFWCT sand mass collection results
#' 
#' @import dplyr
#' @param df_in Data frame of sand mass data.
#' @return Data frame of sumamrized results.
summarize_sandmass <- function(df_in, wetness, period){
  df_in$trgtwet <- as.integer(gsub("%", "", df_in$treatment))
  treat_sum <- df_in %>% group_by(dca, trgtwet) %>% 
    summarize(avg.sand.mass=mean(sand.mass)) %>% ungroup() %>%
    left_join(select(wetness, dca, trgtwet, dryness), by=c("dca", "trgtwet"))
  control_sum <- treat_sum %>% group_by(dca) %>%
    do(control.mass=filter(., trgtwet==0)$avg.sand.mass,
       control.dry=filter(., trgtwet==0)$dryness)
  control_sum[control_sum$dca=="T13", 2] <- NA
  control_sum[control_sum$dca=="T13", 3] <- NA
  control_sum$control.mass <- unlist(control_sum$control.mass)
  control_sum$control.dry <- unlist(control_sum$control.dry)
  treat_sum <- inner_join(treat_sum, control_sum, by="dca") %>%
    mutate(control.eff=1-((avg.sand.mass*dryness)/(control.mass*control.dry)))
  treat_sum$control.eff <- round(treat_sum$control.eff, 2) * 100
  treat_sum[treat_sum$trgtwet==0, ]$control.eff <- NA
  treat_sum$avg.sand.mass <- round(treat_sum$avg.sand.mass, 2)
  treat_sum
}

#' Assign points to DCM
#' 
#' @import dplyr
#' @param df_in. Data frame of points with location in *x, y* columns.
#' @param poly_df. Data frame of vertex points defining polygons. Point 
#' locations in *x, y* columns, *objectid* column identifies individual polygons.
#' @return Original df_in data frame with extra *objectid* column indicating 
#' which polygon the point is interior to. NA in the *objectid* indicated that 
#' the point is not interior to any of the polygons defined in poly_df. 
assign_dcm <- function(df_in, poly_df=poly_df){
  for (i in 1:nrow(df_in)){
    poly_index <- NA
    for (j in unique(poly_df$objectid)){
      polycheck <- sp::point.in.polygon(df_in$x[i], df_in$y[i], 
                                    dplyr::filter(poly_df, objectid==j)$x, 
                                    dplyr::filter(poly_df, objectid==j)$y)
      if (polycheck==1){
        poly_index <- j
        break
      }
    }
    df_in$objectid[i] <- poly_index
  }
  df_in
}

clean_ce <- function(x){
  x <- as.character(x)
  x <- sapply(x, function(x) ifelse((x=="Inf" | x=="-Inf" | is.na(x) 
                                     | x=="NaN"), 
                                    "NA", x))
}

