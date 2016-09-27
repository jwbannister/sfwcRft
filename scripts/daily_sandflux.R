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
  summarize(sand.flux=round(sum(sand_flux), 2)) %>%
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
                                                    
filtered_flux <- daily_flux %>%
  filter(!(dca=='T26' & period=='1115' & csc %in% intrusion_list[['1115']])) %>%
  filter(!(dca=='T26' & period=='1215' & csc %in% intrusion_list[['1215']])) %>%
  filter(!(dca=='T26' & period=='0216' & csc %in% intrusion_list[['0216']])) %>%
  filter(!(dca=='T26' & period=='0316' & csc %in% intrusion_list[['0316']])) %>%
  filter(!(dca=='T26' & period=='0416' & csc %in% intrusion_list[['0416']])) %>%
  filter(!(dca=='T26' & period=='0516' & csc %in% intrusion_list[['0516']])) 

geom_adj <- 1.2 #sandcatch geometry adjustment for sandflux calculation
cutoff <- 1
daily_mass <- filtered_flux %>% group_by(day, treatment, dca) %>% 
  summarize(sand.mass=round(mean(sand.flux*geom_adj), 1)) %>%
  arrange(day) %>% ungroup()
daily_mass$greater1 <- ifelse(daily_mass$sand.mass>cutoff, TRUE, FALSE)
qualified_days <- daily_mass %>% filter(treatment=="0%") %>%
  select(-sand.mass, -treatment) 

flux_summary <- summarize_flux_sfwct(daily_flux, wet_record) %>%
  left_join(qualified_days, by=c("dca", "day")) %>%
  rename(qualified = greater1, period=period.x)
flux_summary$period <- ordered(flux_summary$period, 
                               levels=c('0415', '0515', '0615', '1115', '1215', 
                                        '0116', '0216', '0316', '0416', '0516', 
                                        '0616'))

avg_daily <- flux_summary %>% 
  select(dca, trgtwet, period, avg.flux) %>%
  group_by(dca, trgtwet, period) %>%
  summarize(avg.daily = round(mean(avg.flux), 2)) %>%
  dcast(dca + trgtwet ~ period) 
write.csv(avg_daily, 
          file="~/dropbox/owens/sfwcrft/code_output/avg_daily_flux.csv",
          row.names=FALSE) 

daily_flux$period <- ordered(daily_flux$period, 
                             levels=c('0415', '0515', '0615', '1115', '1215', 
                                      '0116', '0216', '0316', '0416', '0516', 
                                      '0616'))
site_daily <- daily_flux %>% 
  select(csc, dca, treatment, period, sand.flux) %>%
  group_by(csc, dca, treatment, period) %>%
  summarize(avg.daily = round(mean(sand.flux), 2)) %>%
  dcast(csc + dca + treatment ~ period) 
write.csv(site_daily, 
          file="~/dropbox/owens/sfwcrft/code_output/site_daily_flux.csv",
          row.names=FALSE) 

daily_ce_tbl <- flux_summary %>% filter(qualified) %>%
  select(dca, trgtwet, period, date=day, ce=control.eff) %>%
  dcast(dca + date ~ trgtwet) %>%
  arrange(dca, date) 
  
daily_ce_melt <- melt(daily_ce_tbl, id.vars=c("dca", "date"), 
                     variable.name="trgtwet", value.name="ce")
daily_ce_melt$period <- paste0(substr(daily_ce_melt$date, 6, 7), 
                               substr(daily_ce_melt$date, 3, 4))
daily_ce_melt$trgtwet <- as.integer(as.character(daily_ce_melt$trgtwet))
cm_summ <- flux_summary %>% filter(qualified) %>%
  mutate(control.mass = control.flux * geom_adj) %>%
  select(dca, day, trgtwet, period, control.mass) 

avg_daily_ce <- daily_ce_melt %>% group_by(dca, trgtwet, period) %>%
  summarize(avg.ce = round(mean(ce), 0))
write.csv(avg_daily_ce, 
          file="~/dropbox/owens/sfwcrft/code_output/avg_daily_ce.csv",
          row.names=FALSE) 

plot_df <- daily_ce_melt %>%
  left_join(cm_summ, by=c("dca", "date"="day", "trgtwet", "period")) %>%
  left_join(wet_record, by=c("dca", "trgtwet", "period"))
plot_df$wet <- plot_df$wet * 100

wet_flux_plot <- plot_df %>% filter(ce>0) %>%
  ggplot(aes(x=wet, y=ce)) +
  geom_point(aes(size=control.mass, color=dca)) +
  ggtitle("Daily Sand Flux Control Efficiency - 2015/2016 Dust Season") +
  scale_colour_manual(name="DCA", values=c("red", "blue", "green", "orange")) +
  scale_size_continuous(name="0% Area Mass") +
  scale_y_continuous(name="Control Efficiency (%)", 
                     breaks=seq(0, 100, 10)) +
  scale_x_continuous(name="Wetness Cover (%)", 
                     breaks=seq(0, 80, 10)) 
png(filename="~/dropbox/owens/sfwcrft/code_output/wet_flux_ce_plot.png", 
    width=8, height=6, units="in", res=300)
print(wet_flux_plot)
dev.off()

daily_ce_tbl$dca <- ordered(daily_ce_tbl$dca, 
                            levels=c("T10", "T26", "T29", "T13"), 
                            labels=c("T10-1b", "T26", "T29-2", "T13-1"))
daily_ce_tbl$date <- format(daily_ce_tbl$date, "%m-%d-%y")
names(daily_ce_tbl) <- c("DCA", "Date", "Target 45%", "Target 55%", 
                         "Target 65%", "Target 75%")
write.csv(daily_ce_tbl, 
          file="~/dropbox/owens/sfwcrft/code_output/daily_ce_table.csv", 
          row.names=F)


                                   
