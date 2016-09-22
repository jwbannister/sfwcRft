rm(list=ls())
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
  group_by(csc, day, period, treatment, dca) %>%  
  summarize(sand.flux=round(sum(sand_flux), 2),
            avg.ws=mean(windspeed_10m), avg.wd=mean(winddirection_10m)) %>%
  ungroup()
tmp <- daily_flux %>% select(csc, treatment, dca)
tmp <- tmp[!duplicated(tmp$csc), ]
expand_daily <- expand.grid(csc=unique(daily_flux$csc), 
                            day=seq(min(daily_flux$day), max(daily_flux$day), 
                                    by="days"), 
                            stringsAsFactors=FALSE) %>%
  mutate(period=paste0(formatC(month(day), width=2, format="d", flag="0"), 
                       substring(year(day), 3))) %>%
  left_join(tmp, by="csc")
full_daily <- left_join(expand_daily, daily_flux, by=c("csc", "day", "period", 
                                                       "treatment", "dca"))
full_daily[is.na(full_daily$sand.flux), "sand.flux"] <- 0
daily_flux <- full_daily
daily_flux <- filter(daily_flux, !(period %in% c("0715", "0815", "0915", 
                                                 "1015")))

geom_adj <- 1.2 #sandcatch geometry adjustment for sandflux calculation
daily_mass <- daily_flux %>% group_by(day, treatment, dca) %>% 
  summarize(sand.mass=round(mean(sand.flux*geom_adj), 1)) %>%
  arrange(day) %>% ungroup()
daily_mass$greater1 <- ifelse(daily_mass$sand.mass>1, TRUE, FALSE)
qualified_days <- daily_mass %>% filter(treatment=="0%") %>%
  select(-sand.mass, -treatment) 

ce_wet_df <- data.frame(dca=ce_wetness[[1]]$dca,
                        trgtwet=ce_wetness[[1]]$trgtwet)
for (i in names(ce_wetness)){
  tmp <- ce_wetness[[i]]
  names(tmp)[3] <- i
  ce_wet_df <- left_join(ce_wet_df, tmp, by=c("dca", "trgtwet"))
}
ce_wet_melt <- melt(ce_wet_df, id.vars=c("dca", "trgtwet"), 
                    variable.name="period", value.name="wetness")
ce_wet_melt$dryness <- 1 - ce_wet_melt$wetness

flux_summary <- summarize_flux_sfwct(daily_flux, ce_wet_melt)

avg_daily <- daily_flux %>%
  select(csc, dca, treatment, period, sand.flux) %>%
  group_by(dca, treatment, period) %>%
  summarize(avg.daily = round(mean(sand.flux), 2))
avg_daily$period <- paste0("p", avg_daily$period)
daily_cast <- dcast(avg_daily, dca + treatment ~ period) %>%
  select(dca, treatment, p0415, p0515, p0615, p1115, p1215, p0116, 
         p0216, p0316, p0416, p0516, p0616)
names(daily_cast) <- c("DCA", "Target Wetness", "Apr 2015", "May 2015", 
                       "June 2015", "Nov 2015", "Dec 2015", "Jan 2016", 
                       "Feb 2016", "Mar 2016", "Apr 2016", "May 2016", 
                       "June 2016")
write.csv(daily_cast, 
          file="~/dropbox/owens/sfwcrft/code_output/avg_daily.csv",
          row.names=FALSE) 

site_daily <- daily_flux %>%
  select(csc, dca, treatment, period, sand.flux) %>%
  group_by(csc, dca, treatment, period) %>%
  summarize(avg.daily = round(mean(sand.flux), 2))
site_daily$period <- paste0("p", site_daily$period)
site_cast <- dcast(site_daily, csc + dca + treatment ~ period) %>%
  select(csc, dca, treatment, p0415, p0515, p0615, p1115, p1215, p0116, 
         p0216, p0316, p0416, p0516, p0616)
names(site_cast) <- c("DCA", "Target Wetness", "Apr 2015", "May 2015", 
                       "June 2015", "Nov 2015", "Dec 2015", "Jan 2016", 
                       "Feb 2016", "Mar 2016", "Apr 2016", "May 2016", 
                       "June 2016")
write.csv(site_cast, 
          file="~/dropbox/owens/sfwcrft/code_output/site_daily.csv",
          row.names=FALSE) 
                       




daily_ce <- flux_summary %>% select(-period.x, -period.y) %>%
  left_join(qualified_days, by=c("dca", "day")) %>%
  filter(greater1) %>% select(-greater1)
ce_table <- daily_ce %>% select(dca, trgtwet, date=day, ce=control.eff) %>%
  reshape2::dcast(dca + date ~ trgtwet) %>%
  arrange(date) %>% filter(!(date=='2015-06-26' & dca=='T26'))
ce_table$dca <- ordered(ce_table$dca, levels=c("T10", "T26", "T29", "T13"), 
                        labels=c("T10-1b", "T26", "T29-2", "T13-1"))
ce_table$date <- format(ce_table$date, "%m-%d-%y")
for (i in 3:6){
  ce_table[ , i] <- sapply(ce_table[ , i],
                           function(x) ifelse(is.na(x), "NA", paste0(x, "%")))
}
names(ce_table) <- c("DCA", "Date", "Target 45%", "Target 55%", "Target 65%", 
                     "Target 75%")
write.csv(ce_table, 
          file="~/dropbox/owens/sfwcrft/code_output/flux_ce_table.csv", 
          row.names=F)
  
tmp <- ce_table
names(tmp) <- c("dca", "date", "t45", "t55", "t65", "t75")
tmp <- tmp %>% 
  reshape2::melt(id.vars=c("dca", "date")) 
tmp$value <- as.numeric(gsub("%", "", tmp$value))
mean_daily_tbl <- tmp %>%
  group_by(dca, variable) %>%
  summarize(mean.wet = round(mean(value), 0))
mean_daily_tbl <- mean_daily_tbl[complete.cases(mean_daily_tbl), ]
mean_daily_tbl$mean.wet <- paste0(mean_daily_tbl$mean.wet, "%")
mean_daily_tbl$variable <- paste0(gsub("t", "", mean_daily_tbl$variable), "%")
names(mean_daily_tbl) <- c("DCA", "Target Wetness", "Average CE")
write.csv(mean_daily_tbl, 
          file="~/dropbox/owens/sfwcrft/code_output/mean_daily_flux.csv", 
          row.names=FALSE)

ce_curve <- data.frame(wetness=c(.72, .64, .28, 0), ce=c(99, 94, 59, 0))
ce_plot_data <- daily_ce
ce_plot_data$t26.filter <- ifelse((ce_plot_data$dca=='T26') & 
                              (ce_plot_data$day<'2016-03-15'), 
                            FALSE, TRUE)
ce_plot_data$wetness <- ce_plot_data$wetness*100
swir_flux_plot <- ce_plot_data %>% filter(control.eff>0, t26.filter) %>%
  rename(ce=control.eff) %>%
  ggplot(aes(x=wetness, y=ce)) +
  geom_point(aes(color=dca, size=control.flux)) +
  scale_colour_manual(name="DCA", 
                      values=c("red", "blue", "green", "orange")) +
  scale_size_continuous(name="0% Area Flux") +
  scale_y_continuous(name="Control Efficiency (%)", breaks=seq(0, 100, 10)) +
  scale_x_continuous(name="SWIR Estimated Wetness Cover (%)", 
                     breaks=seq(0, 80, 10)) 
png(filename="~/dropbox/owens/sfwcrft/code_output/swir_flux_plot.png", 
    width=8, height=6, units="in",res=300) 
print(swir_flux_plot)
dev.off()

report_flux <- daily_ce %>% 
  select(-avg.flux, -wetness, -dryness, -control.flux, -control.dry)
report_flux$trgtwet <- paste0("t_", report_flux$trgtwet)
a <- select(daily_ce, dca, day, control.flux)
a <- a[!duplicated(a), ]
a$control.flux <- a$control.flux * geom_adj
names(a)[3] <- "control.mass"
report_cast <- dcast(report_flux, dca + day ~ trgtwet) %>% 
  left_join(a, by=c("dca", "day")) %>%
  select(dca, day, control.mass, t_45, t_55, t_65, t_75)
save(report_cast, file="./data/flux.RData")

                                   
