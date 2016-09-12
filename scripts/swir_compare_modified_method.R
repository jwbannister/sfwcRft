# This script compares the modified DRI SWIR adjustement (LandSat adjusted) to 
# the atmospheric adjustment only (by Formation)
load_all()
load_all("~/code/owensData")
library(rgdal)
library(raster)
library(ggplot2)
library(dplyr)
rasterOptions(tolerance = 1)

t26 <- readOGR(path.expand("~/code/data/sfwcRft/T26"), 
               "T26_sfwct_boundaries")
ras_uniform <- raster("~/dropbox/data/swir/Formation_noadj/113015_form_noadj.tif")
ras_uniform[ , ] <- 1
t26_ras <- ras_clip(ras_uniform, t26)
t26_trgtwet_ras <- ras_clip(ras_uniform, t26, fld="TrgtWet")
trgt_index <- c(0, 45, 55, 65, 75)
zones <- vector(mode="list", length=length(trgt_index))
names(zones) <- trgt_index
for (i in names(zones)){
  temp <- t26_trgtwet_ras
  temp[ , ] <- sapply(temp[ , ], function(x) ifelse(x==i, 1, NA))
  zones[[i]] <- temp
}

index <- c("041615", "060815", "062015", "113015")
results <- vector(mode="list", length=length(index))
names(results) <- index
for (j in index){
  ras_form <- raster(paste0("~/dropbox/data/swir/Formation_noadj/", j, 
                            "_form_noadj.tif"))
  ras_form <- ras_form * t26_ras
  wet_form <- class_wet(ras_form)
  ras_dri <- raster(paste0("~/dropbox/data/swir/DRI_LSadj/", j, 
                           "_LSadj.tif"))
  ras_dri <- ras_dri * t26_ras
  wet_dri <- class_wet(ras_dri)
  form_data <- data.frame(img=rep("No Adjustment", length(ras_form[ , ])), 
                          swir=ras_form[ , ])
  dri_data <- data.frame(img=rep("LandSat Adjusted", length(ras_dri[ , ])), 
                         swir=ras_dri[ , ])
  results[[j]]$cdf.t26 <- rbind(form_data, dri_data) %>%
    ggplot(aes(x=swir)) + 
    stat_ecdf(aes(color=img)) +
    ggtitle(paste0("All of T26, Image Date = ", j)) +
    theme(legend.title=element_blank(), 
          legend.position=c(.8, .2))
  results[[j]]$wetness <- data.frame(trgtwet=c(), img=c(), wetness=c())
  for (k in names(zones)){
    form_tmp <- ras_form * zones[[k]]
    dri_tmp <- ras_dri * zones[[k]]
    form_tmp_data <- data.frame(trgtwet=rep(k, length(form_tmp[ , ])), 
                                img=rep("No Adjustment", length(form_tmp[ , ])), 
                                swir=form_tmp[ , ])
    dri_tmp_data <- data.frame(trgtwet=rep(k, length(dri_tmp[ , ])), 
                               img=rep("LandSat Adjusted", length(dri_tmp[ , ])), 
                               swir=dri_tmp[ , ])
    data_tmp <- rbind(form_tmp_data, dri_tmp_data)
    results[[j]][[paste0("cdf.", k)]] <- data_tmp %>%
      ggplot(aes(x=swir)) +
      stat_ecdf(aes(color=img)) +
      ggtitle(paste0("Treatment = ", k, "%, Image Date = ", j)) +
      theme(legend.title=element_blank(), 
            legend.position=c(.8, .2))
    form_tmp_wet <- wet_form * zones[[k]]
    form_wetness <- sum(form_tmp_wet[ , ], na.rm=T)/sum(!is.na(form_tmp_wet[ , ]))
    dri_tmp_wet <- wet_dri * zones[[k]]
    dri_wetness <- sum(dri_tmp_wet[ , ], na.rm=T)/sum(!is.na(dri_tmp_wet[ , ]))
    temp <- data.frame(trgtwet=k,
                       img=c("No Adjustment", "LandSat Adjusted"), 
                       wetness=c(form_wetness, dri_wetness))
    results[[j]]$wetness <- rbind(results[[j]]$wetness, temp)
  }
}

library(gridExtra)
wetness_sum <- data.frame(date=c(), trgtwet=c(), img=c(), wetness=c())
for (m in names(results)){
  g1 <- arrangeGrob(results[[m]]$cdf.t26, 
                    results[[m]]$cdf.0, 
                    results[[m]]$cdf.45, 
                    results[[m]]$cdf.55, 
                    results[[m]]$cdf.65, 
                    results[[m]]$cdf.75, ncol=3)
  ggsave(filename=paste0(m, "_reflect_CDFs.pdf"), plot=g1, 
         path="~/dropbox/SWIR Comparison", height=9.5, width=15, units="in", 
         device="pdf")
  temp <- cbind(data.frame(date=rep(m, nrow(results[[m]]$wetness)), 
                           results[[m]]$wetness))
  wetness_sum <- rbind(wetness_sum, temp)
}
wetness_sum$date <- ordered(wetness_sum$date)
wetness_sum$wetness <- round(wetness_sum$wetness, 3)
p2 <- wetness_sum %>% 
  ggplot(aes(x=date, y=wetness, group=interaction(trgtwet, img))) +
  geom_path(aes(color=img)) +
  facet_grid(trgtwet ~ ., scales="free_y") +
  theme(legend.title=element_blank())
pdf(file="~/dropbox/SWIR Comparison/SWIR processing comparison.pdf", 
    paper="letter")
print(p2)
dev.off()

query1 <- "SELECT dcm, trgtwet, day, SUM(round) AS wetness 
           FROM sfwct.wetness_summary 
           WHERE class='W' 
           AND day IN ('2015-11-30', '2015-12-01', '2016-04-26', '2016-05-27')
           GROUP BY dcm, trgtwet, day
           ORDER BY day, trgtwet;"
ground <- query_owenslake(query1) %>% select(-dcm)
ground$date <- paste0(substr(ground$day, 6, 7), substr(ground$day, 9, 10), 
                      substr(ground$day, 3, 4))
ground$date <- ordered(ground$date, 
                       levels=c('113015', '120115', '042616', '052716'))
ground$img <- rep("Ground Obs", nrow(ground))
ground <- select(ground, date, trgtwet, img, wetness)

index <- c("113015", "120115", "042616", "052716")
results2 <- vector(mode="list", length=length(index))
names(results2) <- index
for (j in index){
  ras_form <- raster(paste0("~/dropbox/data/swir/Formation_noadj/", j, 
                            "_form_noadj.tif"))
  ras_form <- ras_form * t26_ras
  wet_form <- class_wet(ras_form)
  form_data <- data.frame(img=rep("No Adjustment", length(ras_form[ , ])), 
                          swir=ras_form[ , ])
  results2[[j]]$wetness <- data.frame(trgtwet=c(), img=c(), wetness=c())
  for (k in names(zones)){
    form_tmp <- ras_form * zones[[k]]
    form_tmp_data <- data.frame(trgtwet=rep(k, length(form_tmp[ , ])), 
                                img=rep("No Adjustment", length(form_tmp[ , ])), 
                                swir=form_tmp[ , ])
    form_tmp_wet <- wet_form * zones[[k]]
    form_wetness <- sum(form_tmp_wet[ , ], na.rm=T)/sum(!is.na(form_tmp_wet[ , ]))
    temp <- data.frame(trgtwet=k,
                       img="No Adjustment", 
                       wetness=form_wetness)
    results2[[j]]$wetness <- rbind(results2[[j]]$wetness, temp)
  }
}
wetness2_sum <- data.frame(date=c(), trgtwet=c(), img=c(), wetness=c())
for (m in names(results2)){
  temp <- cbind(data.frame(date=rep(m, nrow(results2[[m]]$wetness)), 
                           results2[[m]]$wetness))
  wetness2_sum <- rbind(wetness2_sum, temp)
}
wetness2_sum$date <- ordered(wetness2_sum$date, 
                       levels=c('113015', '120115', '042616', '052716'))
wetness2_sum$wetness <- round(wetness2_sum$wetness, 3)

fill <- data.frame(date=c('113015', '120115', '042616', '052716'), 
                   trgtwet=c(0, 0, 0, 0), 
                   img=rep("Ground Obs", 4), 
                   wetness=c(0, 0, 0, 0))
ground_comp <- rbind(ground, wetness2_sum, fill) %>%
  arrange(date, trgtwet, img)
p3 <- ground_comp %>% 
  ggplot(aes(x=date, y=wetness, group=interaction(trgtwet, img))) +
  geom_path(aes(color=img)) +
  facet_grid(trgtwet ~ ., scales="free_y") +
  theme(legend.title=element_blank())
pdf(file="~/dropbox/SWIR Comparison/SWIR-ground comparison.pdf", 
    paper="letter")
print(p3)
dev.off()

write.csv(wetness_sum, row.names=F, 
          file="~/dropbox/SWIR Comparison/SWIR processing comparison.csv")
write.csv(ground_comp, row.names=F, 
          file="~/dropbox/SWIR Comparison/SWIR-ground comparison.csv")
