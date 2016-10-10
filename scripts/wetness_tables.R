rm(list=ls())
load_all()
load_all("~/code/owensData")
library(tidyverse)
library(ggplot2)
library(reshape2)

swir_tbl <- wet_record %>% filter(method=='swir') %>% select(-method) %>%
  dcast(dca + trgtwet ~ period)

tran_melt <- tran_record %>% select(dca, trgtwet, period, wet) 
tran_melt$period <- ordered(tran_melt$period, 
                            levels=c('0515', '1115', '1215', '0116', '0316',
                                     '0416', '0516'))
tran_tbl <- tran_melt %>% arrange(period) %>% dcast(dca + trgtwet ~ period)
tran_tbl <- rbind(tran_tbl, c("T13", 75, rep(NA, 7)))
tran_tbl <- rbind(tran_tbl, c("T29", 0, rep(NA, 3), 0, rep(NA, 3)))
tran_tbl <- arrange(tran_tbl, dca, trgtwet)
tran_tbl$trgtwet <- as.integer(tran_tbl$trgtwet)

swir_trans <- inner_join(melt(swir_tbl, id.vars=c("dca", "trgtwet"), 
                              variable.name="period", value.name="swir"), 
                         melt(tran_tbl, id.vars=c("dca", "trgtwet"), 
                              variable.name="period", value.name="tran"), 
                         by=c("dca", "trgtwet", "period"))
swir_trans$tran <- as.numeric(swir_trans$tran)

# write table of wetness for use in dustReport
wet_tmp <- wet_record %>% filter(method=="swir") %>%
  select(period, dca, trgtwet, swir=wet)
tran_tmp <- tran_record %>% 
  select(period, dca, trgtwet, trans=wet)
full_wet <- select(wet_record, period, dca, trgtwet) %>%
  left_join(wet_tmp, by=c("period", "dca", "trgtwet")) %>%
  left_join(tran_tmp, by=c("period", "dca", "trgtwet"))
save(full_wet, file="~/code/sfwcRft/data/wet_table.RData")

composite_tbl <- cbind(swir_tbl[ , 1:7], tran_tbl[ , 6], tran_tbl[ , 6:7], 
                       swir_tbl[ , 8:10])
names(composite_tbl)[8:9] <- c("0116", "0216")
write.csv(composite_tbl, 
          file="~/dropbox/owens/sfwcrft/code_output/composite_table.csv", 
          row.names=F)

ab_df <- data.frame(int=0, slp=1)
swir_trans_plot <- swir_trans[complete.cases(swir_trans), ] %>%
  ggplot(aes(x=swir*100, y=tran*100)) +
  geom_point(aes(color=dca)) +
  scale_x_continuous(name="SWIR Estimated Wetness (%)", 
                     breaks=seq(0, 100, 10)) +
  scale_y_continuous(name="Ground Estimated Wetness (%)", 
                     breaks=seq(0, 100, 10)) +
  scale_color_brewer(name="DCA", palette="Set1") +
  geom_abline(data=ab_df, mapping=aes(intercept=int, slope=slp, 
                                      linetype="1-1 Line"), color="blue") +
  scale_linetype(name="")
png(filename="~/dropbox/owens/sfwcrft/code_output/swir_trans_plot.png", 
    height=6, width=6, units="in", res=300)
swir_trans_plot
dev.off()

swir_tmp <- swir_trans %>% mutate(var=paste0("swir.", period)) %>%
  select(-period, -tran) %>%
  dcast(dca + trgtwet ~ var, value.var='swir')
tran_tmp <- swir_trans %>% mutate(var=paste0("tran.", period)) %>%
  select(-period, -swir) %>%
  dcast(dca + trgtwet ~ var, value.var='tran')
swir_trans_tbl <- inner_join(swir_tmp, tran_tmp, by=c("dca", "trgtwet"))
write.csv(swir_trans_tbl, 
          file="~/dropbox/owens/sfwcrft/code_output/swir_trans_table.csv", 
          row.names=F)

names(swir_tbl) <- c("DCA", "Target Wetness", "Apr 16, 2015", "June 8, 2015", 
                     "June 20, 2015", "Nov 30, 2015", "Dec 1, 2015", 
                     "Apr 26, 2016", "May 27, 2016", "June 24, 2016")
swir_tbl$DCA[swir_tbl$DCA=="T10"] <- "T10-1b"
swir_tbl$DCA[swir_tbl$DCA=="T29"] <- "T29-2"
swir_tbl$DCA[swir_tbl$DCA=="T13"] <- "T13-1"
swir_tbl$'Target Wetness' <- paste0(swir_tbl$'Target Wetness', "%")
for (i in 3:10){
  swir_tbl[ , i] <- swir_tbl[ , i] * 100
}
write.csv(swir_tbl, 
          file="~/dropbox/owens/sfwcrft/code_output/swir_table.csv", 
          row.names=F)

names(tran_tbl) <- c("DCA", "Target Wetness", "May 2015", "November 2015", 
                     "December 2015", "January 2016", "March 2016", 
                     "April 2016", "May 2016")
tran_tbl$DCA[tran_tbl$DCA=="T10"] <- "T10-1b"
tran_tbl$DCA[tran_tbl$DCA=="T29"] <- "T29-2"
tran_tbl$DCA[tran_tbl$DCA=="T13"] <- "T13-1"
tran_tbl$'Target Wetness' <- paste0(tran_tbl$'Target Wetness', "%")
for (i in 3:9){
  tran_tbl[ , i] <- tran_tbl[ , i] * 100
}
write.csv(tran_tbl, 
          file="~/dropbox/owens/sfwcrft/code_output/transect_table.csv", 
          row.names=F)

library(gridExtra)
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

wet_plot_df <- composite_tbl
wet_plot_melt <- reshape2::melt(wet_plot_df, id.vars=c("dca", "trgtwet"))
wet_plot_melt$variable <- ordered(wet_plot_melt$variable, 
                                   levels=c("0415", "0515", "0615", "1115", 
                                            "1215", "0116", "0216", "0316", 
                                            "0416", "0516", "0616"))
                                            
wet_plot_melt[wet_plot_melt$dca=='T26' & 
               wet_plot_melt$variable=='0415', ]$value <- NA
wet_plot_melt$trgtwet <- as.numeric(gsub("%", "", wet_plot_melt$trgtwet)) 
wet_plot_melt$value <- as.numeric(gsub("%", "", wet_plot_melt$value)) 
wet_plot_melt1 <- wet_plot_melt 
wet_plot_melt1[wet_plot_melt1$variable %in% c("0116", "0216", "0316"), ]$value <- NA
wet_plot_melt1[wet_plot_melt1$dca=='T26' & wet_plot_melt1$variable=='0515', ]$value <- NA
wet_plot_melt1[wet_plot_melt1$dca=='T29' & (wet_plot_melt1$variable %in% c('0415', '0515', '0615')), ]$value <- NA

wet_tracking <- vector(mode="list", length=4)
names(wet_tracking) <- c("T10-1b", "T26", "T13-1", "T29-2")
for (i in names(wet_tracking)){
  dca_df <- wet_plot_melt1 %>% filter(dca==substr(i, 1, 3))
  wet_tracking[[i]] <- vector(mode="list", 
                             length=length(unique(dca_df$trgtwet)))
  names(wet_tracking[[i]]) <- unique(dca_df$trgtwet)
  for (j in unique(dca_df$trgtwet)){
    plot_df <- dca_df %>% filter(trgtwet==j)
    ref_lines <- data.frame(type=c("Target Wetness", "Average SWIR Wetness"), 
                            intercept=c(j*0.01, mean(plot_df$value, na.rm=T)),
                            slope=c(0, 0), x=wet_plot_melt$variable[1])
    avg_wet <- mean(plot_df$value)
    wet_tracking[[i]][[as.character(j)]] <- plot_df %>% 
      ggplot(aes(x=variable, y=value)) +
      geom_abline(mapping=aes(intercept=intercept, linetype=type, slope=slope), 
                  data=ref_lines, color="red") + 
      geom_path(aes(group=dca, color="SWIR Image Result")) +
      theme(axis.text.y=element_blank(), 
#            axis.text.x=element_blank(),
            axis.title.x=element_blank(), 
            axis.title.y=element_blank(), 
            axis.ticks.x=element_blank(), 
            axis.ticks.y=element_blank(), 
            plot.title=element_text(size=8), 
            legend.position="none", 
            legend.title=element_blank()) + 
      scale_color_manual(name="", values=c("blue")) + 
      scale_y_continuous(breaks=seq(0, 1, .1), limits=c(0, 1)) + 
      scale_x_discrete(labels=c("A", "M", "J", "N", "D", "J", "F", "M", "A", 
                                "M", "J")) +
      ggtitle(paste0(i, " ", j, "%"))
  }
}

#lgnd <- g_legend(wet_tracking[[1]][[1]])
#print(grid.arrange(lgnd))
#leg.file <- tempfile()
#save(lgnd, file=leg.file)

png(filename="~/Desktop/swir_panel.png",
    height=6, width=10, units="in", res=300)
grid.arrange(wet_tracking[[1]][[1]], 
             wet_tracking[[1]][[2]], 
             wet_tracking[[1]][[3]], 
             wet_tracking[[1]][[4]], 
             wet_tracking[[1]][[5]], 
             wet_tracking[[2]][[1]], 
             wet_tracking[[2]][[2]], 
             wet_tracking[[2]][[3]], 
             wet_tracking[[2]][[4]], 
             wet_tracking[[2]][[5]], 
             wet_tracking[[3]][[1]], 
             wet_tracking[[3]][[2]], 
             wet_tracking[[4]][[1]], 
             wet_tracking[[4]][[2]], 
             lgnd, nrow=3)
dev.off()
