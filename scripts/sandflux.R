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
full_daily <- inner_join(expand_daily, daily_flux, by=c("csc", "day"))
full_daily[is.na(full_daily$sand.flux), "sand.flux"] <- 0
daily_flux <- full_daily
daily_flux$period <- paste0(substr(daily_flux$day, 6, 7), 
                            substr(daily_flux$day, 3, 4))
daily_flux <- filter(daily_flux, !(period %in% c("0715", "0815", "0915")))

ce_wet_df <- data.frame(dca=ce_wetness[[1]]$dca,
                        trgtwet=ce_wetness[[1]]$trgtwet)
for (i in names(ce_wetness)){
  tmp <- ce_wetness[[i]]
  names(tmp)[3] <- i
  ce_wet_df <- left_join(ce_wet_df, tmp, by=c("dca", "trgtwet"))
}
ce_wet_melt <- melt(ce_wet_df, id.vars=c("dca", "trgtwet"), 
                    variable.name="period", value.name="wetness")
ce_wet_melt$dryness <- ce_wet_melt

flux_summary <- summarize_flux_sfwct(daily_flux, ce_wet_melt)
daily_ce <- flux_summary %>% select(-period.x, -period.y)

ce_curve <- data.frame(wetness=c(.72, .64, .28, 0), ce=c(99, 94, 59, 0))
swir_mass_plot <- daily_ce %>% 
  rename(ce=control.eff) %>%
  ggplot(aes(x=wetness, y=ce)) +
  geom_point(aes(color=trgtwet)) +
  geom_curve(data=ce_curve, mapping=aes(x=wetness[4], y=ce[4], 
                                        linetype="SIP CE Curve", 
                                        xend=wetness[1], yend=ce[1]),
             curvature=-.2, color="red") +
  ggtitle("T26 SFWCRFT Sand Mass Control Efficiency\n2015/2016 Dust Season") +
  scale_colour_manual(name="Target Wetness", 
                      values=c("red", "blue", "green", "orange")) +
  scale_linetype(name="")

png(filename="~/Desktop/swir_mass_plot.png", width=8, height=6, units="in", 
    res=300)
print(swir_mass_plot)
dev.off()

