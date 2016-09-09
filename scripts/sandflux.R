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
  summarize(sand.flux=round(sum(sand_flux), 2)) %>% ungroup()
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
daily_flux <- filter(daily_flux, !(period %in% c("0715", "0815", "0915")))

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
daily_ce <- flux_summary %>% select(-period.x, -period.y) %>%
  left_join(qualified_days, by=c("dca", "day")) %>%
  filter(greater1) %>% select(-greater1)

print(length(unique(filter(daily_ce, dca=="T10", !is.na(control.eff))$day)))
print(length(unique(filter(daily_ce, dca=="T26", !is.na(control.eff))$day)))
print(length(unique(filter(daily_ce, dca=="T29", !is.na(control.eff))$day)))

flux_melt <- vector(mode="list", length=3)
names(flux_melt) <- unique(daily_ce$dca)
for (i in unique(daily_ce$dca)){
flux_melt[[i]] <- daily_ce %>% filter(dca==i) %>%
  select(dca, trgtwet, day, ce=control.eff) %>%
  dcast(dca + day ~ trgtwet)
}
write.csv(flux_melt[['T26']], file="~/Desktop/t26_flux.csv", row.names=F)

ce_curve <- data.frame(wetness=c(.72, .64, .28, 0), ce=c(99, 94, 59, 0))
swir_flux_plot <- daily_ce %>% filter(control.eff>0) %>%
  rename(ce=control.eff) %>%
  ggplot(aes(x=wetness, y=ce)) +
  geom_point(aes(color=dca, size=control.flux)) +
  ggtitle("Daily Sand Flux Control Efficiency\n2015/2016 Dust Season") +
  geom_smooth(method="glm", family=gaussian(link="log")) + 
  scale_colour_manual(name="DCA", 
                      values=c("red", "blue", "green", "orange")) +
  scale_size_continuous(name="0% Area Flux")


png(filename="~/Desktop/swir_flux_plot.png", width=8, height=6, units="in", 
    res=300)
print(swir_flux_plot)
dev.off()

