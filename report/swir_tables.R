rm(list=ls())
load_all()
load_all("~/code/owensData")
library(rgdal)
library(raster)
library(ggplot2)
library(dplyr)

swir_df <- data.frame(dca=wetness[[1]]$dca, trgtwet=wetness[[1]]$trgtwet)
for (i in names(wetness)){
  tmp <- wetness[[i]]
  names(tmp)[3] <- i
  swir_df <- left_join(swir_df, tmp, by=c("dca", "trgtwet"))
}
swir_melt <- reshape2::melt(swir_df, id.vars=c("dca", "trgtwet"), 
                            variable.name="date", 
                            value.name="swir")
swir_tbl <- swir_df[ , names(swir_df)!="050915"]
swir_tbl$dca <- ordered(swir_tbl$dca, levels=c("T10", "T26", "T29", "T13"))
swir_tbl <- arrange(swir_tbl, dca, trgtwet)
swir_tbl$dca <- as.character(swir_tbl$dca)
names(swir_tbl) <- c("DCA", "Target Wetness", "Apr 16, 2015", "June 8, 2015", 
                     "June 20, 2015", "Nov 30, 2015", "Dec 1, 2015", 
                     "Apr 26, 2016", "May 27, 2016", "June 24, 2016")
swir_tbl$DCA[swir_tbl$DCA=="T10"] <- "T10-1b"
swir_tbl$DCA[swir_tbl$DCA=="T29"] <- "T29-2"
swir_tbl$DCA[swir_tbl$DCA=="T13"] <- "T13-1"
swir_tbl$'Target Wetness' <- paste0(swir_tbl$'Target Wetness', "%")
for (i in 3:10){
  swir_tbl[ , i] <- paste0(swir_tbl[ , i] * 100, "%")
}
# remove areas when not in operation
swir_tbl[swir_tbl$DCA=='T26', 3] <- NA
swir_tbl[swir_tbl$DCA=='T29-2', 3:5] <- NA
write.csv(swir_tbl, file="./report/output/swir_target_table.csv", row.names=F)

query1 <- "SELECT dcm as dca, trgtwet, day, round AS wet
           FROM sfwct.wetness_summary 
           WHERE class='W'"
trans_summ <- query_owenslake(query1)
# add in 0% wetness in T26 0% area for all transect days (this is what was 
# measured, but since wetness transect has 0 length for these areas, it will not 
# show up in summary view
a <- unique(filter(trans_summ, dca=='T26')$day)
tmp <- data.frame(dca=rep('T26', length(a)), trgtwet=rep(0, length(a)), 
                  day=a, wet=rep(0, length(a)))
trans_summ <- rbind(trans_summ, tmp)
trans_summ$date <- paste0(substr(trans_summ$day, 6, 7), 
                          substr(trans_summ$day, 9, 10), 
                          substr(trans_summ$day, 3, 4))
trans_summ$trgtwet <- as.character(trans_summ$trgtwet)
swir_trans <- left_join(select(trans_summ, -day), swir_melt, 
                        by=c("dca", "trgtwet", "date"))
swir_trans <- swir_trans %>% filter(date!='060815', !is.na(swir))
swir_trans_melt <- reshape2::melt(swir_trans, 
                                  id.vars=c("dca", "trgtwet", "date"))
swir_trans_melt$date <- ordered(swir_trans_melt$date, 
                                levels=c("113015", "120115", "042616", 
                                         "052716"))
swir_trans_plot <- swir_trans %>%
  ggplot(aes(x=swir*100, y=wet*100)) +
  geom_point() +
  scale_x_continuous(name="SWIR Estimated Wetness (%)", 
                     breaks=seq(0, 100, 10)) +
  scale_y_continuous(name="Ground Estimated Wetness (%)", 
                     breaks=seq(0, 100, 10)) +
  geom_abline(intercept=0, slope=1, color="blue") 
png(filename="./report/output/swir_trans_plot.png", height=6, width=6, 
    units="in", res=300)
print(swir_trans_plot)
dev.off()

swir_trans_melt$variable <- paste0(swir_trans_melt$date, ".", 
                                   swir_trans_melt$variable)
swir_trans_cast <- swir_trans_melt %>% select(-date) %>%
  reshape2::dcast(dca + trgtwet ~ variable) %>%
  arrange(dca, trgtwet)
swir_trans_cast$trgtwet <- paste0(swir_trans_cast$trgtwet, "%")
for (i in 3:10){
  swir_trans_cast[ , i] <- 
    sapply(swir_trans_cast[ , i], 
           function(x) ifelse(is.na(x), "NA", paste0(x * 100, "%")))
}
write.csv(swir_trans_cast, file="./report/output/swir_trans_table.csv", 
         row.names=FALSE) 
