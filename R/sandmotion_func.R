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

summarize_flux_sfwct <- function(df1){
  df1$treatment <- paste0("t_", substr(df1$dcm, 5, nchar(df1$dcm)-1))
  df1$area <- substr(df1$dcm, 1, 3)
  df2 <- df1 %>% group_by(area, treatment, day) %>% 
    summarize(sand=mean(sand.flux)) %>% ungroup()
  df_temp <- summarize_ce(df2) %>% arrange(day, area) %>%
    filter(control.sand>1)
  if (nrow(df_temp)==0) {
    return("No days with control area sand flux > 1")
  } else{
  df_out <- dcast(df_temp, area + day + control.sand ~ treatment,
                  value.var="control.eff") %>%
  filter(!is.na(t_45)) %>% select(-t_0) %>% group_by(area) %>%
  arrange(desc(control.sand))
  }
  df_out
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
summarize_sandmass <- function(df_in){
  df_in$treatment <- paste0("t_", df_in$treatment)
  treat_sum <- df_in %>% group_by(dca, treatment) %>% 
    summarize(avg.sand.mass=mean(sand.mass)) %>% ungroup()
  control_sum <- treat_sum %>% group_by(dca) %>%
    do(control.mass=filter(., treatment=="t_0%")$avg.sand.mass)
  control_sum[control_sum$dca=="T13", 2] <- NA
  control_sum$control.mass <- unlist(control_sum$control.mass)
  treat_sum <- inner_join(treat_sum, control_sum, by="dca") %>%
    mutate(control.eff=1-(avg.sand.mass/control.mass)) %>% 
    select(-control.mass)
  treat_sum$control.eff <- round(treat_sum$control.eff, 2) * 100
  treat_sum[treat_sum$treatment=="t_0%", ]$control.eff <- NA
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

