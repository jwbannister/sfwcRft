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

swir_trans <- inner_join(melt(swir_tbl, id.vars=c("dca", "trgtwet"), 
                              variable.name="period", value.name="swir"), 
                         melt(tran_tbl, id.vars=c("dca", "trgtwet"), 
                              variable.name="period", value.name="tran"), 
                         by=c("dca", "trgtwet", "period"))

swir_trans_plot <- swir_trans %>%
  ggplot(aes(x=swir*100, y=tran*100)) +
  geom_point() +
  scale_x_continuous(name="SWIR Estimated Wetness (%)", 
                     breaks=seq(0, 100, 10)) +
  scale_y_continuous(name="Ground Estimated Wetness (%)", 
                     breaks=seq(0, 100, 10)) +
  geom_abline(intercept=0, slope=1, color="blue") 
png(filename="~/dropbox/owens/sfwcrft/code_output/swir_trans_plot.png", 
    height=6, width=6, units="in", res=300)
print(swir_trans_plot)
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

swir_plot_df <- swir_tbl
names(swir_plot_df) <- c("dca", "trgtwet", "04/16/2015", "06/08/2015", 
                         "06/20/2015", "11/30/2015", "12/01/2015",
                         "04/26/2016", "05/27/2016", "06/24/2016")
swir_plot_melt <- reshape2::melt(swir_plot_df, id.vars=c("dca", "trgtwet"))
swir_plot_melt$variable <- ordered(swir_plot_melt$variable, 
                                   levels=c("04/16/2015", "06/08/2015", 
                                            "06/20/2015", "11/30/2015", 
                                            "12/01/2015", "04/26/2016",
                                            "05/27/2016", "06/24/2016"))
swir_plot_melt[swir_plot_melt$dca=='T26' & 
               swir_plot_melt$variable=='04/16/2015', ]$value <- NA
swir_plot_melt$trgtwet <- as.numeric(gsub("%", "", swir_plot_melt$trgtwet)) 
swir_plot_melt$value <- as.numeric(gsub("%", "", swir_plot_melt$value)) 
swir_tracking <- vector(mode="list", length=4)
names(swir_tracking) <- c("T10-1b", "T26", "T13-1", "T29-2")
for (i in names(swir_tracking)){
  dca_df <- swir_plot_melt %>% filter(dca==i)
  swir_tracking[[i]] <- vector(mode="list", 
                             length=length(unique(dca_df$trgtwet)))
  names(swir_tracking[[i]]) <- unique(dca_df$trgtwet)
  for (j in unique(dca_df$trgtwet)){
    plot_df <- dca_df %>% filter(trgtwet==j)
    ref_lines <- data.frame(type=c("Target Wetness", "Average SWIR Wetness"), 
                            intercept=c(j, mean(plot_df$value, na.rm=T)),
                            slope=c(0, 0), x=swir_plot_melt$variable[1])
    avg_wet <- mean(plot_df$value)
    swir_tracking[[i]][[as.character(j)]] <- plot_df %>% 
      ggplot(aes(x=variable, y=value)) +
      geom_abline(mapping=aes(intercept=intercept, linetype=type, slope=slope), 
                  data=ref_lines, color="red") + 
      geom_path(aes(group=dca, color="SWIR Image Result")) +
      theme(axis.text.x=element_blank(), 
            axis.text.y=element_blank(),
            axis.title.x=element_blank(), 
            axis.title.y=element_blank(), 
            axis.ticks.x=element_blank(), 
            axis.ticks.y=element_blank(), 
            plot.title=element_text(size=8), 
            legend.position="none", 
            legend.title=element_blank()) + 
      scale_color_manual(name="", values=c("blue")) + 
      scale_y_continuous(breaks=seq(0, 100, 10), limits=c(0, 100)) + 
      ggtitle(paste0(i, " ", j, "%"))
  }
}

#lgnd <- g_legend(swir_tracking[[1]][[1]])
#print(grid.arrange(lgnd))
#leg.file <- tempfile()
#save(lgnd, file=leg.file)

png(filename="~/dropbox/owens/sfwcrft/code_output/swir_panel.png", 
    height=6, width=10, units="in", res=300)
grid.arrange(swir_tracking[[1]][[1]], 
             swir_tracking[[1]][[2]], 
             swir_tracking[[1]][[3]], 
             swir_tracking[[1]][[4]], 
             swir_tracking[[1]][[5]], 
             swir_tracking[[2]][[1]], 
             swir_tracking[[2]][[2]], 
             swir_tracking[[2]][[3]], 
             swir_tracking[[2]][[4]], 
             swir_tracking[[2]][[5]], 
             swir_tracking[[3]][[1]], 
             swir_tracking[[3]][[2]], 
             swir_tracking[[4]][[1]], 
             swir_tracking[[4]][[2]], 
             lgnd, nrow=3)
dev.off()
