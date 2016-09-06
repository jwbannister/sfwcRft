load_all()
load_all("~/code/owensData")
library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)

start.date <- mdy("04-01-2015")
end.date <- mdy("06-30-2016")
descrip <- "Shallow Flood Wetness Cover Refinement Field Test"
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
                  period=paste0(month, "-", year))

geom_adj <- 1.2 #sandcatch geometry adjustment for sandflux calculation
monthly_mass <- flux_df %>% group_by(csc, month, year, period, area, treatment, dca) %>% 
  summarize(sand.mass=round(sum(sand_flux)*geom_adj, 1)) %>%
  arrange(year, month) %>% ungroup()

mass_summary <- vector(mode="list", 
                       length=length(unique(monthly_mass$period)) + 1)
names(mass_summary) <- c(unique(monthly_mass$period), "season")
dust.season <- c("10-2015", "11-2015", "12-2015", "1-2016", "2-2016", "3-2016",
                 "4-2016", "5-2016", "6-2016")
for (i in 1:length(mass_summary)){
  if (i==length(mass_summary)){
    mass_summary[[i]] <- monthly_mass %>%
      filter(period %in% dust.season) %>%
      summarize_sandmass()
    mass_summary[[i]]$period <- names(mass_summary)[i]
  } else{
    mass_summary[[i]] <- monthly_mass %>%
      filter(period==names(mass_summary)[i]) %>%
      summarize_sandmass()
    mass_summary[[i]]$period <- names(mass_summary)[i]
  }
  mass_summary[[i]]$control.eff <- clean_ce(mass_summary[[i]]$control.eff)
}

areas <- unique(monthly_mass$dca)
contour_plots <- vector(mode="list", length=length(areas))
names(contour_plots) <- areas
join_mass <- left_join(monthly_mass, csc_locs, by="csc")
for (i in areas){
  p1 <- plot_dcm_background(i, sfwct_polys, sfwct_labels)
  for (j in unique(monthly_mass$period)){
    contour_plots[[i]][[j]] <- plot_csc_masses(p1, join_mass, i, j)
  }
}

p1 <- monthly_mass %>% filter(area=="T26") %>%
  ggplot(aes(x=period, y=sand.mass, group=csc)) +
  geom_path(aes(color=csc)) +
  facet_grid(treatment ~ .)

daily_flux <- flux_df %>% 
  mutate(day = as.Date(ymd(substring(datetime, 1, 10)))) %>%
  group_by(csc, day, month, year, period, windspeed_10m, winddirection_10m, 
           area, treatment, dca) %>% 
  summarize(sand.flux=round(sum(sand_flux), 2)) %>% ungroup()
expand_daily <- expand.grid(csc=unique(daily_flux$csc), 
                            day=unique(daily_flux$day), 
                            stringsAsFactors=FALSE)
expand_daily <- left_join(expand_daily, select(daily_flux, csc, dca), 
                           by="csc") %>% arrange(csc, day)
expand_daily <- expand_daily[!duplicated(expand_daily), ]
full_daily <- left_join(expand_daily, daily_flux, by=c("csc", "day", "dca"))
full_daily[is.na(full_daily$sand.flux), "sand.flux"] <- 0
daily_flux <- full_daily

flux_summary <- summarize_flux_sfwct(daily_flux), 


for (i in 1:nrow(flux_summary)){
  dat <- filter(wind_data[[flux_summary$area[i]]],
                as.Date(ymd(substring(datetime, 1, 10)))==flux_summary$day[i])
  max_wind <- ifelse(nrow(dat)>0, max(dat$windspeed_10m), NA)
  flux_summary$max.windspeed[i] <- ifelse(nrow(dat)>0, round(max_wind, 1), NA)
  wind_dir <- arrange(dat, desc(windspeed_10m)) %>% 
    filter(!is.na(winddirection_10m))
  flux_summary$winddirection[i] <- ifelse(nrow(wind_dir)>0, 
                                          wind_dir$winddirection_10m[1],
                                          NA)
}

