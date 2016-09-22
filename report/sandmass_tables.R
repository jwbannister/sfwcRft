rm(list=ls())
load_all()
load_all("~/code/owensData")
load_all("~/code/owensMaps")
library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)
library(reshape2) 

start.date <- mdy("04-01-2015")
end.date <- mdy("06-30-2016")
descrip <- "Shallow Flood Wetness Cover Refinement Field Test"
flux_df <- pull_5min_flux(start.date, end.date, descrip)
csc_locs <- pull_csc_locs(descrip)

dust_season <- c("0415", "0515", "0615", "1115", "1215", "0116", "0216", 
                 "0316", "0416", "0516", "0616")

geom_adj <- 1.2 #sandcatch geometry adjustment for sandflux calculation
monthly_mass <- flux_df %>% group_by(csc, month, year, period, area, treatment, dca) %>% 
  summarize(sand.mass=round(sum(sand_flux)*geom_adj, 1)) %>%
  arrange(year, month) %>% ungroup()

mass_tbl <- monthly_mass %>% filter(period %in% dust_season) %>%
  group_by(dca, treatment, period) %>%
  summarize(avg.mass = round(mean(sand.mass), 2)) %>% ungroup() 
mass_tbl1 <- mass_tbl
mass_tbl1$period <- paste0("p", mass_tbl1$period)
mass_tbl_cast <- dcast(mass_tbl1, dca + treatment ~ period) %>%
  select(dca, treatment, p0415, p0515, p0615, p1115, p1215, p0116, p0216, 
        p0316, p0416, p0516, p0616) 
names(mass_tbl_cast) <- c("DCA", "Target Wetness", "Apr 2015", "May 2015", 
                     "June 2015", "Nov 2015", "Dec 2015", "Jan 2015", 
                     "Feb 2015", "Mar 2015", "Apr 2015", "May 2015", 
                     "June 2015")
write.csv(mass_tbl_cast, file="~/dropbox/owens/sfwcrft/code_output/avg_mass.csv", 
          row.names=F)

site_mass_tbl <- monthly_mass %>% filter(period %in% dust_season) %>%
  select(csc, dca, treatment, period, sand.mass)
site_mass_tbl$period <- paste0("p", site_mass_tbl$period)
site_mass_tbl <- dcast(site_mass_tbl, csc + dca + treatment ~ period) %>%
  select(csc, dca, treatment, p0415, p0515, p0615, p1115, p1215, p0116, p0216, 
         p0316, p0416, p0516, p0616) %>%
  arrange(dca, treatment, csc)
names(site_mass_tbl) <- c("CSC", "DCA", "Target Wetness", "Apr 2015", "May 2015", 
                     "June 2015", "Nov 2015", "Dec 2015", "Jan 2015", 
                     "Feb 2015", "Mar 2015", "Apr 2015", "May 2015", 
                     "June 2015")
write.csv(site_mass_tbl, 
          file="~/dropbox/owens/sfwcrft/code_output/site_mass.csv", 
          row.names=F)
  

mass_summary <- vector(mode="list", length=length(dust_season))
names(mass_summary) <- dust_season
for (i in names(mass_summary)){
  calc_wetness <- ce_wetness[[i]]
  calc_wetness$dryness <- 1 - calc_wetness[ , 3]
  mass_summary[[i]] <- monthly_mass %>%
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
ce_table <- ce_summ %>% filter(trgtwet!='0', dca!='T13')
ce_table$dca <- ordered(ce_table$dca, levels=c("T10", "T26", "T29", "T13"), 
                        labels=c("T10-1b", "T26", "T29-2", "T13-1"))
ce_table <- arrange(ce_table, dca, trgtwet)
for (i in 2:13){
  ce_table[ , i] <- sapply(as.integer(ce_table[ , i]), 
                           function(x) ifelse(is.na(x), "NA", 
                                              paste0(x, "%")))
}
names(ce_table) <- c("DCA", "Target Wetness", "Apr 2015", "May 2015", 
                     "June 2015", "Nov 2015", "Dec 2015", "Jan 2016", 
                     "Feb 2016", "Mar 2016", "Apr 2016", "May 2016", 
                     "June 2016")
write.csv(ce_table, 
          file="~/dropbox/owens/sfwcrft/code_output/mass_ce_table.csv", 
          row.names=F)

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

swir_ce_plot <- swir_ce %>% filter(ce>0, !is.na(swir), !is.na(ce)) %>%
  rename(wetness=swir) %>%
  ggplot(aes(x=wetness, y=ce)) +
  geom_point(aes(size=control.mass, shape=t26.filter, color=dca)) +
  scale_colour_manual(name="DCA", values=c("red", "blue", "green", "orange")) +
  scale_size_continuous(name="0% Area Mass") +
  scale_shape_manual(name="T26 Sand Fence", values=c(4, 19), 
                     labels=c("Pre-install", "Post-install")) +
  scale_y_continuous(name="Control Efficiency (%)", 
                     breaks=seq(0, 100, 10)) +
  scale_x_continuous(name="SWIR Estimated Wetness Cover (%)", 
                     breaks=seq(0, 80, 10)) 
png(filename="~/dropbox/owens/sfwcrft/code_output/mass_ce_plot.png", 
    width=8, height=6, units="in", res=300)
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

## find best fit curve y = a - (b/(x - d)^g)
#coefficients <- data.frame(a=c(), b=c(), d=c(), g=c(), rms=c())
#for (a in seq(100, 103, 0.1)){
#  print(a)
#  for(b in seq(8000, 12000, 500)){
#    print(b)
#    for (d in seq(2, 8, 0.1)){
#      for (g in seq(1.5, 3, 0.1)){
#        err <- apply(swir_ce[!is.na(swir_ce$swir) & swir_ce$swir>20 & 
#                     !is.na(swir_ce$ce), 
#                     c('swir', 'ce')], 1, 
#                     function(x) x[2] - a - b/((x[1] - d)^g))
#        rms <- sqrt(mean(err^2))
#        coefficients <- rbind(coefficients, 
#                              data.frame(a=a, b=b, d=d, g=g, rms=rms))
#      }
#    }
#  }
#}

ce_melt <- melt(ce_summ, id.vars=c("dca", "trgtwet"), variable.name="period", 
                value.name="ce")
ce_melt$trgtwet <- paste0(ce_melt$trgtwet, "%")
report_mass <- left_join(mass_tbl, ce_melt, by=c("dca", "treatment"="trgtwet", 
                                                 "period"))
save(report_mass, file="./data/mass.RData")
