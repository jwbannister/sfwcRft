rm(list=ls())
load_all()
load_all("~/code/owensData")
load_all("~/code/owensMaps")
library(tidyverse)
library(lubridate)
library(hms)
library(ggplot2)

start.date <- mdy("04-01-2015")
end.date <- mdy("06-30-2016")
descrip <- "Shallow Flood Wetness Cover Refinement Field Test"
flux_df <- pull_5min_flux(start.date, end.date, descrip)

df1 <- flux_df 
df1$hour <- hour(df1$datetime)
df1$date <- as.Date(substr(df1$datetime, 1, 10))
df2 <- df1 %>% select(-datetime, -area, -month, -year, -sensit) %>%
  group_by(date, hour, period, csc, dca, treatment) %>%
  summarize(sand.flux=sum(sand_flux, na.rm=T), 
            avg.ws=mean(windspeed_10m), 
            avg.wd=mean(winddirection_10m)) %>% ungroup()
expand_hours <- expand.grid(date=seq(start.date, end.date, "days"), 
                            hour=c(0:23), csc=unique(df2$csc))
df3 <- left_join(expand_hours, df2, by=c("date", "hour", "csc")) %>%
  select(-period, -dca, -treatment) %>%
  left_join(unique(select(df2, csc, dca, treatment)), by="csc")
df3[is.na(df3$sand.flux), ]$sand.flux <- 0
hourly_flux <- df3
hourly_flux$period <- paste0(substr(hourly_flux$date, 6, 7), 
                             substr(hourly_flux$date, 3, 4))

# filter out CSC sites which experienced sand intrusion from control plot,   
intrusion_list <- vector(mode="list", length=0)
intrusion_list[['1115']] <- c('1540', '1538', '1537', '1536', '1535', '1534',
                           '1533', '1532', '1531','1524', '1530', '1522',
                           '1521', '1520', '1510')
intrusion_list[['1215']] <- c('1540', '1538', '1537', '1535', '1534', '1533',
                           '1532', '1531', '1530')
intrusion_list[['0216']] <- c('1538', '1534', '1532', '1531', '1530', '1524',
                           '1522', '1521', '1520')
intrusion_list[['0316']] <- c('1535', '1522', '1521', '1520')
intrusion_list[['0416']] <- c('1521', '1522', '1523', '1513', '1520', '1511',
                           '1512')
intrusion_list[['0516']] <- c('1530', '1524', '1523', '1522', '1521')
                                                    
filtered_flux <- hourly_flux %>%
  filter(!(dca=='T26' & period=='1115' & csc %in% intrusion_list[['1115']])) %>%
  filter(!(dca=='T26' & period=='1215' & csc %in% intrusion_list[['1215']])) %>%
  filter(!(dca=='T26' & period=='0216' & csc %in% intrusion_list[['0216']])) %>%
  filter(!(dca=='T26' & period=='0316' & csc %in% intrusion_list[['0316']])) %>%
  filter(!(dca=='T26' & period=='0416' & csc %in% intrusion_list[['0416']])) %>%
  filter(!(dca=='T26' & period=='0516' & csc %in% intrusion_list[['0516']])) 

filtered_flux <- filter(filtered_flux, !(period %in% c("0715", "0815", "0915", 
                                                 "1015")))

geom_adj <- 1.2 #sandcatch geometry adjustment for sandflux calculation
daily_mass <- filtered_flux %>% group_by(date, treatment, dca, csc) %>% 
  summarize(daily.mass=sum(sand.flux*geom_adj)) %>% ungroup() %>%
  group_by(date, treatment, dca) %>%
  summarize(avg.daily=mean(daily.mass)) %>% ungroup()
daily_mass$greater1 <- ifelse(daily_mass$avg.daily>1, TRUE, FALSE)
qualified_days <- daily_mass %>% filter(treatment=="0%") %>%
  select(-avg.daily, -treatment) 

flux_summary <- summarize_flux_hourly(hourly_flux, wet_record)
hourly_ce <- flux_summary %>% select(-period.x, -period.y) %>%
  left_join(qualified_days, by=c("dca", "date")) %>%
  filter(greater1) %>% select(-greater1, -method.x, -method.y, -wet)
hourly_ce$control.eff <- sapply(hourly_ce$control.eff, 
                                function(x) ifelse(is.finite(x), x, NA))

hourly_ce_tbl <- hourly_ce %>% arrange(desc(control.flux)) %>%
  mutate(t0.flux=round(control.flux, 2)) %>%
  select(date, hour, dca, trgtwet, t0.flux, control.eff)
hourly_ce_tbl <- filter(hourly_ce_tbl, 
                        t0.flux>sort(unique(hourly_ce_tbl$t0.flux))[50])
hourly_ce_tbl <- spread(hourly_ce_tbl, trgtwet, control.eff) %>%
  arrange(desc(t0.flux))
write.csv(hourly_ce_tbl, 
          file="~/dropbox/owens/sfwcrft/code_output/hourly_ce_tbl.csv", 
          row.names=FALSE)
hourly_flux_tbl <- filtered_flux %>% arrange(desc(sand.flux)) %>%
  mutate(ws=round(avg.ws, 1), wd=round(avg.wd, 0), flux=round(sand.flux, 2)) %>%
  select(csc, date, hour, dca, treatment, ws, wd, flux)
write.csv(hourly_flux_tbl[1:50, ], 
          file="~/dropbox/owens/sfwcrft/code_output/hourly_flux_tbl.csv", 
          row.names=FALSE)
  




