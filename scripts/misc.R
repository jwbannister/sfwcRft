rm(list=ls())
load_all()
load_all("~/code/owensData")
load_all("~/code/owensMaps")
library(tidyverse)
library(rgdal)
library(lubridate)
library(hms)

# pull and summarize hourly sand flux data in T26 for Kent
start.date <- mdy("07-01-2015")
end.date <- mdy("06-30-2016")
descrip <- "Shallow Flood Wetness Cover Refinement Field Test"
flux_df <- pull_5min_flux(start.date, end.date, descrip)

df1 <- flux_df %>% filter(dca=='T26', treatment=='0%')
df1$hour <- hour(df1$datetime)
df1$date <- as.Date(substr(df1$datetime, 1, 10))
df2 <- df1 %>% select(date, hour, csc, sand_flux)
df3 <- df2 %>% group_by(date, hour, csc) %>%
  summarize(sand.flux=sum(sand_flux, na.rm=T))

expand_hours <- expand.grid(date=seq(start.date, end.date, "days"), 
                            hour=c(0:23), csc=unique(df2$csc))
df4 <- left_join(expand_hours, df3, by=c("date", "hour", "csc"))
df4[is.na(df4$sand.flux), ]$sand.flux <- 0
df4$csc <- paste0("csc_", df4$csc)
  
hourly_tbl <- df4 %>% arrange(date, hour, csc) %>%
  spread(csc, sand.flux)

write.csv(hourly_tbl, file="~/dropbox/to KENT/T26_0_hourly_flux.csv", 
          row.names=FALSE)

# build a table of instrument counts by sfwct area
inst_locs <- 
  readOGR(path.expand("~/dropbox/gis/owens/SFWCT/SFWCT_final_locations"), 
                      "SFWCT_final_locations")@data
xtabs(~ desc + DCA, data=inst_locs)
