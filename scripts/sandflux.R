load_all()
load_all("~/code/owensData")
load_all("~/code/owensMaps")
library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)

start.date <- mdy("04-01-2015")
end.date <- mdy("06-30-2016")
descrip <- "Shallow Flood Wetness Cover Refinement Field Test"
flux_df <- pull_5min_flux(start.date, end.date, descrip)

daily_flux <- flux_df %>% 
  mutate(day = as.Date(ymd(substring(datetime, 1, 10)))) %>%
  group_by(csc, day, month, year, period, windspeed_10m, winddirection_10m, 
           area, treatment, dca) %>% 
  summarize(sand.flux=round(sum(sand_flux), 2)) %>% ungroup()
expand_daily <- expand.grid(csc=unique(daily_flux$csc), 
                            day=unique(daily_flux$day), 
                            stringsAsFactors=FALSE)
expand_daily <- expand_daily[!duplicated(expand_daily), ]
full_daily <- left_join(expand_daily, daily_flux, by=c("csc", "day"))
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

