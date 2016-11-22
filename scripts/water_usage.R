rm(list=ls())
load_all()
library(tidyverse)
library(ggplot2)

sched <- read.csv("./data/sprinkler_schedules.csv") %>%
  gather(lateral, minutes, -date)
lats <- read.csv("./data/lateral_breakdown.csv")
lats$galpmin <- lats$heads * lats$galpminphead
lats$dca <- substr(lats$lateral, 1, 3)

area_sum <- lats %>% filter(treatment!=0) %>%
  group_by(dca, treatment) %>%
  summarize(galpmin = sum(galpmin), 
            acres = sum(acres)) %>%
  mutate(galpminpacre = galpmin / acres)

daily_volume <- sched %>% full_join(select(lats, lateral, treatment, galpmin), 
                                    by="lateral") %>%
  mutate(gal = galpmin * minutes, 
         dca = substr(lateral, 1, 3)) %>%
  filter(!is.na(date))

area_summary_daily <- daily_volume %>% group_by(date, dca, treatment) %>%
  summarize(gal = sum(gal)) %>% ungroup() %>% filter(treatment!=0) %>%
  # units: 3.07 * 10^-6 acre-ft = 1 gallon
  mutate(acreft = gal * (3.07 * 10^-6)) %>%
  left_join(select(area_sum, dca, treatment, acres), 
            by=c("dca", "treatment")) %>%
  # units: 12 in / ft
  mutate(inches.h2o = (acreft / acres) * 12, 
         month = paste0(substr(date, 1, 2), "-", substr(date, 7, 8)))

area_summary_monthly <- area_summary_daily %>% 
  group_by(dca, treatment, month) %>%
  summarize(acreft = sum(acreft), 
            inches.h2o = sum(inches.h2o)) %>%
  left_join(select(area_sum, dca, treatment, acres), 
            by=c("dca", "treatment"))

area_summary_monthly$month <- ordered(area_summary_monthly$month, 
                                      levels=c("06-15", "07-15", "08-15", 
                                               "09-15", "10-15", "11-15", 
                                               "12-15", "01-16", "02-16",
                                               "03-16", "04-16", "05-16", 
                                               "06-16"), 
                                      labels=c("Jun15", "Jul15", "Aug15", 
                                               "Sep15", "Oct15", "Nov15", 
                                               "Dec15", "Jan16", "Feb16",
                                               "Mar16", "Apr16", "May16", 
                                               "Jun16"))
area_summary_monthly$treatment <- ordered(area_summary_monthly$treatment, 
                                      levels=c(45, 55, 65, 75))
area_summary_monthly$afpapy <- (area_summary_monthly$acreft / 
                                area_summary_monthly$acres) * 12

p1 <- area_summary_monthly %>% arrange(month) %>%
  ggplot(aes(x=month, y=afpapy)) +
  geom_point(aes(color=treatment)) +
  geom_path(aes(group=treatment, color=treatment)) +
  facet_grid(dca ~ .) +
  ylab("Acre-Ft/Year/Acre") + xlab("Month") +
  scale_colour_brewer("Treatment", palette="Set1") +
  theme(axis.text.x=element_text(angle=90))
png(filename="~/dropbox/owens/2015-2016 sfwcrft/code_output/water_usage_plot.png", 
    height=6, width=6, units="in", res=300)
p1
dev.off()

area_summary_monthly$afpapy <- round(area_summary_monthly$afpapy, 2)
area_summary_monthly$acreft <- round(area_summary_monthly$acreft, 2)
usage_csv <- area_summary_monthly %>% arrange(dca, treatment, month) %>%
  select(dca, treatment, month, acreft) %>%
  spread(month, acreft)
write.csv(usage_csv, 
          file="~/dropbox/owens/2015-2016 sfwcrft/code_output/water_usage.csv",
          row.names=F)
rate_csv <- area_summary_monthly %>% arrange(dca, treatment, month) %>%
  select(dca, treatment, month, afpapy) %>%
  spread(month, afpapy)
write.csv(rate_csv, 
          file="~/dropbox/owens/2015-2016 sfwcrft/code_output/water_rate.csv",
          row.names=F)

meter <- read.csv("./data/meter.csv") %>% 
  gather(lateral, meter.acreft, -reading)
readings <- data.frame(reading = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                       start = as.Date(c("05-29-15", "06-29-15", "07-31-15", 
                                 "08-31-15", "09-30-15", "11-03-15", 
                                 "11-30-15", "12-30-15", "02-23-16", 
                                 "03-17-16", "04-18-16", "05-10-16", 
                                 "06-21-16"), format="%m-%d-%y"), 
                       end = as.Date(c("06-28-15", "07-30-15", "08-30-15", "09-29-15", 
                               "11-02-15", "11-29-15", "12-29-15", "02-22-16", 
                               "03-18-16", "04-17-16", "05-09-16", "06-20-16", 
                               "07-18-16"), format="%m-%d-%y"))

lateral_summary_daily <- daily_volume %>% filter(dca=='T26') %>%
  group_by(date, lateral) %>%
  summarize(gal = sum(gal)) %>% ungroup() %>% 
  # units: 3.07 * 10^-6 acre-ft = 1 gallon
  mutate(acreft = gal * (3.07 * 10^-6)) 
lateral_summary_daily$date <- as.Date(lateral_summary_daily$date, 
                                      format="%m/%d/%y")
lateral_summary_daily$reading <- rep(NA, nrow(lateral_summary_daily))
for (i in 1:nrow(lateral_summary_daily)){
  for (j in 1:nrow(readings)){
    if (between(lateral_summary_daily$date[i], 
                readings$start[j], readings$end[j])){
      lateral_summary_daily$reading[i] <- readings$reading[j]
    }
  }
}

meter_compare <- lateral_summary_daily %>% filter(reading >= 5) %>%
  group_by(reading, lateral) %>%
  summarize(acreft=round(sum(acreft), 0)) %>%
  left_join(meter, by=c("reading", "lateral"))
write.csv(meter_compare, 
          file="~/dropbox/owens/2015-2016 sfwcrft/code_output/meter_compare.csv",
          row.names=F)

p2 <- meter_compare %>% 
  ggplot(aes(x=acreft, y=meter.acreft)) +
  geom_point() +
  geom_abline(intercept=0, slope=1, color="red") +
  geom_text(aes(x=12, y=5, label="1-1 Line"), color="red") + 
  ylab("Meter Volume (acre-ft)") + xlab("Calculated Volume (acre-ft)")
png(filename="~/dropbox/owens/2015-2016 sfwcrft/code_output/meter_plot.png", 
    height=6, width=6, units="in", res=300)
p2
dev.off()




