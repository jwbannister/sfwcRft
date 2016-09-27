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
csc_locs <- pull_csc_locs(descrip)

geom_adj <- 1.2 #sandcatch geometry adjustment for sandflux calculation
monthly_mass <- flux_df %>% group_by(csc, month, year, period, area, treatment, dca) %>% 
  summarize(sand.mass=round(sum(sand_flux)*geom_adj, 1)) %>%
  arrange(year, month) %>% ungroup()

# filter out CSC sites which experienced sand intrusion from control plot,   
# months that are not part of the dust season 
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
                                                    
filtered_mass <- monthly_mass %>%
  filter(!(dca=='T26' & period=='1115' & csc %in% intrusion_list[['1115']])) %>%
  filter(!(dca=='T26' & period=='1215' & csc %in% intrusion_list[['1215']])) %>%
  filter(!(dca=='T26' & period=='0216' & csc %in% intrusion_list[['0216']])) %>%
  filter(!(dca=='T26' & period=='0316' & csc %in% intrusion_list[['0316']])) %>%
  filter(!(dca=='T26' & period=='0416' & csc %in% intrusion_list[['0416']])) %>%
  filter(!(dca=='T26' & period=='0516' & csc %in% intrusion_list[['0516']])) 

dust_season <- c("0415", "0515", "0615", "1115", "1215", "0116", "0216", 
                 "0316", "0416", "0516", "0616")
filtered_mass <- filtered_mass %>% filter(period %in% dust_season) %>%
  select(-area, -month, -year)
filtered_mass$period <- ordered(filtered_mass$period, 
                                levels=c('0415', '0515', '0615', '1115', 
                                         '1215', '0116', '0216', '0316', 
                                         '0416', '0516', '0616'))

# create table of monthly sand mass by CSC site
site_mass <- filtered_mass %>%
  dcast(csc + dca + treatment ~ period, value.var='sand.mass')
write.csv(site_mass, 
          file="~/dropbox/owens/sfwcrft/code_output/site_mass.csv", 
          row.names=F)

# create table of area-average monthly sand mass by area & treatment
avg_mass <- filtered_mass %>% group_by(dca, treatment, period) %>%
  summarize(avg.mass = round(mean(sand.mass), 2)) %>%
  dcast(dca + treatment ~ period)
write.csv(avg_mass, 
          file="~/dropbox/owens/sfwcrft/code_output/avg_mass.csv", 
          row.names=F)

# calculate control efficiencies
prds <- unique(filtered_mass$period)
for (i in 1:length(prds)){
  prd <- prds[i]
  wet_tmp <- filter(wet_record, period==prd)
  wet_tmp$dryness <- 1 - wet_tmp$wet
  tmp_summary <- filtered_mass %>% filter(period==prd) %>%
    summarize_sandmass(., wetness=wet_tmp, period=prd)
  tmp_summary$period <- prd
  ifelse(i > 1, ce_summary <- rbind(ce_summary, tmp_summary), 
         ce_summary <- tmp_summary)
}

# filter data, only consider months with minimum level of sand motion
cutoff <- 10 # minimum 0% area monthly sand mass for inclusion in results
for (i in 1:nrow(ce_summary)){
  ce_summary$control.eff[i] <- ifelse(ce_summary$control.mass[i] < cutoff, 
                                      NA, ce_summary$control.eff[i])
}

# create table of monthly sand mass reduction control efficiencies
mass_ce_tbl <- ce_summary %>% 
  select(dca, trgtwet, period, control.eff) %>%
  dcast(dca + trgtwet ~ period)
write.csv(mass_ce_tbl, 
          file="~/dropbox/owens/sfwcrft/code_output/mass_ce_tbl.csv", 
          row.names=F)

# plot monthly mass CE vs. wetness cover
mass_ce_melt <- melt(mass_ce_tbl, id.vars=c("dca", "trgtwet"), 
                     variable.name="period", value.name="ce")
cm_summ <- ce_summary %>% select(dca, trgtwet, period, control.mass) %>%
  filter(control.mass > cutoff)
plot_df <- wet_record %>%
  left_join(mass_ce_melt, by=c("dca", "trgtwet", "period")) %>%
  left_join(cm_summ, by=c("dca", "trgtwet", "period"))
plot_df$wet <- plot_df$wet * 100
wet_mass_plot <- plot_df %>% filter(ce>0) %>%
  ggplot(aes(x=wet, y=ce)) +
  geom_point(aes(size=control.mass, color=dca)) +
  ggtitle("Monthly Sand Mass Control Efficiency - 2015/2016 Dust Season") +
  scale_colour_manual(name="DCA", values=c("red", "blue", "green", "orange")) +
  scale_size_continuous(name="0% Area Mass") +
  scale_y_continuous(name="Control Efficiency (%)", 
                     breaks=seq(0, 100, 10)) +
  scale_x_continuous(name="Wetness Cover (%)", 
                     breaks=seq(0, 80, 10)) 
png(filename="~/dropbox/owens/sfwcrft/code_output/wet_mass_ce_plot.png", 
    width=8, height=6, units="in", res=300)
print(wet_mass_plot)
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

        
