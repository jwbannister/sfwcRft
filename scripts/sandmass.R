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
csc_locs <- pull_csc_locs(descrip)

geom_adj <- 1.2 #sandcatch geometry adjustment for sandflux calculation
monthly_mass <- flux_df %>% group_by(csc, month, year, period, area, treatment, dca) %>% 
  summarize(sand.mass=round(sum(sand_flux)*geom_adj, 1)) %>%
  arrange(year, month) %>% ungroup()

# filter out CSC sites which experienced sand intrusion from control plot
filtered_mass <- monthly_mass %>%
  filter(!(dca=='T26' & period=='1115' & csc %in% c('1540', '1538', '1537', 
                                                    '1536', '1535', '1534',
                                                    '1533', '1532', '1531',
                                                    '1524', '1530', '1522', 
                                                    '1521', '1520', '1510'))) %>%
  filter(!(dca=='T26' & period=='1215' & csc %in% c('1540', '1538', '1537', 
                                                    '1535', '1534', '1533',
                                                    '1532', '1531', '1530'))) %>%
  filter(!(dca=='T26' & period=='0216' & csc %in% c('1538', '1534', '1532', 
                                                    '1531', '1530', '1524',
                                                    '1522', '1521', '1520'))) %>%
  filter(!(dca=='T26' & period=='0316' & csc %in% c('1535', '1522', '1521', 
                                                    '1520'))) %>%
  filter(!(dca=='T26' & period=='0416' & csc %in% c('1521', '1522', '1523', 
                                                    '1513', '1520', '1511', 
                                                    '1512'))) %>%
  filter(!(dca=='T26' & period=='0516' & csc %in% c('1530', '1524', '1523', 
                                                    '1522', '1521')))

dust_season <- c("0415", "0515", "0615", "1115", "1215", "0116", "0216", 
                 "0316", "0416", "0516", "0616")
mass_summary <- vector(mode="list", length=length(dust_season))
names(mass_summary) <- dust_season
for (i in names(mass_summary)){
  calc_wetness <- ce_wetness[[i]]
  calc_wetness$dryness <- 1 - calc_wetness[ , 3]
  mass_summary[[i]] <- filtered_mass %>%
    filter(period==i) %>%
    summarize_sandmass(., wetness=calc_wetness, period=i)
  mass_summary[[i]]$period <- i
  mass_summary[[i]]$control.eff <- clean_ce(mass_summary[[i]]$control.eff)
}

cutoff <- 10 # minimum 0% area monthly sand mass for inclusion in results
ce_summ <- data.frame(dca=mass_summary[[10]]$dca, 
                          trgtwet=mass_summary[[10]]$trgtwet)
control_mass_summ <- data.frame(dca=mass_summary[[10]]$dca, 
                          trgtwet=mass_summary[[10]]$trgtwet)
for (i in names(mass_summary)){
  tmp <- mass_summary[[i]] %>% filter(control.mass > cutoff) %>%
    select(dca, trgtwet, control.eff)
  names(tmp)[3] <- i
  ce_summ <- left_join(ce_summ, tmp, by=c("dca", "trgtwet"))
}
write.csv(ce_summ, file="~/Desktop/mass_ce_summary.csv", row.names=F)

control_mass_summ <- data.frame(dca=mass_summary[[10]]$dca, 
                          trgtwet=mass_summary[[10]]$trgtwet)
for (i in names(mass_summary)){
  tmp <- mass_summary[[i]] %>% 
    select(dca, trgtwet, control.mass)
  names(tmp)[3] <- i
  control_mass_summ <- left_join(control_mass_summ, tmp, by=c("dca", "trgtwet"))
}
cm_summ <- melt(control_mass_summ, id.vars=c("dca", "trgtwet"), 
                value.name="control.mass")

swir_ce <- swir_summ %>% select(-average) %>% 
  melt(id.vars=c("dca", "trgtwet"), value.name="swir") %>%
  left_join(melt(ce_summ, id.vars=c("dca", "trgtwet"), value.name="ce"),
            by=c("dca", "trgtwet", "variable"), suffix=c(".swir", ".wet")) %>%
  left_join(cm_summ, by=c("dca", "trgtwet", "variable")) 
swir_ce$ce <- as.numeric(swir_ce$ce)

ce_curve <- data.frame(wetness=c(.72, .64, .28, 0), ce=c(99, 94, 59, 0))
swir_ce$t26.filter <- ifelse((swir_ce$dca=='T26') & 
                            !(swir_ce$variable %in% c("0316", "0416", "0516", "0616")), 
                            FALSE, TRUE)
swir_ce$swir <- swir_ce$swir*100

swir_ce_plot <- swir_ce %>% filter(ce>0, !is.na(swir), !is.na(ce), t26.filter) %>%
  rename(wetness=swir) %>%
  ggplot(aes(x=wetness, y=ce)) +
  geom_point(aes(size=control.mass, color=dca)) +
  ggtitle("Monthly Sand Mass Control Efficiency - 2015/2016 Dust Season") +
  scale_colour_manual(name="DCA", values=c("red", "blue", "green", "orange")) +
  scale_size_continuous(name="0% Area Mass") +
#  stat_function(xlim=c(20, 80), linetype="dashed", color="black", 
#                fun = function(x) 100 - 10000/((x - 5)^2.4)) +
  scale_y_continuous(name="Control Efficiency (%)", 
                     breaks=seq(0, 100, 10)) +
  scale_x_continuous(name="SWIR Estimated Wetness Cover (%)", 
                     breaks=seq(0, 80, 10)) 

png(filename="~/Desktop/swir_ce_plot.png", width=8, height=6, units="in", 
    res=300)
print(swir_ce_plot)
dev.off()

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

for (i in names(contour_plots$T26)[8:15]){
  png(filename=paste0("~/Desktop/T26 contours/", i, ".png"), 
      width=6, height=6, units="in", res=300)
  print(contour_plots$T26[[i]])
  dev.off()
}

        
