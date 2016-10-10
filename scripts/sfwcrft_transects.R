load_all("~/code/owensData")
library(tidyverse)
library(ggplot2)
library(rgdal)

index <- c("1130", "1201", "0426", "0527")
pth1 <- path.expand("~/dropbox/owens/sfwcrft/transect-swir compare/wet_paths/")
pth2 <- path.expand("~/dropbox/owens/sfwcrft/transect-swir compare/transects/")

for (i in index){
    paths_df <- readOGR(pth2, paste0(i, "_wet_transect"))@data
    path_lengths <- paths_df %>% group_by(id) %>%
        summarize(length=sum(length)) %>% ungroup()
    paths_sum <- paths_df %>% group_by(id, class) %>%
        summarize(length=sum(length)) %>% ungroup() %>%
        left_join(path_lengths, by="id") %>%
        mutate(cover=round(length.x/length.y, 2))
    paths_sum$cover <- paths_sum$cover * 100


    swir_df <- readOGR(pth1, paste0(i, "_single"))@data
    swir_areas <- swir_df %>% group_by(id) %>%
        summarize(area=sum(area)) %>% ungroup()
    swir_sum <- swir_df %>% group_by(id, DN) %>%
        summarize(area=sum(area)) %>% ungroup() %>%
        left_join(swir_areas, by="id") %>%
        mutate(cover=round(area.x/area.y, 2)) %>%
        filter(DN==100)
    swir_sum$cover <- swir_sum$cover * 100

    swir_plt <- data.frame(x=c(), xend=c(), swir_w=c())
    for (j in unique(swir_sum$id)){
        temp  <- data.frame(x=j-0.3, xend=j+0.3, 
                            swir_w=filter(swir_sum, id==j)$cover)
        swir_plt <- rbind(swir_plt, temp)
    }
    plot_df <- filter(paths_sum, class!="D")
    plot_df$class <- ordered(plot_df$class, levels=c("M", "W"), 
                             labels=c("Moist", "Wet"))

    p2 <- plot_df[order(plot_df$class, decreasing=T), ] %>% 
        ggplot(aes(x=id, y=cover)) +
        geom_bar(stat="identity", aes(fill=class), color="black") +
        scale_x_continuous(name="Transect", breaks=seq(1, max(plot_df$id), 1)) +
        scale_y_continuous(name="", breaks=c(0, 20, 40, 60, 80, 100), 
                           minor_breaks=seq(0, 100, 5), limits=c(0, 105), 
                           expand=c(0, 0), labels=c("0%", "20%", "40%", "60%", 
                                                    "80%", "100%")) +
scale_fill_manual(name="Ground", values=c("#33a02c", "#0000f6")) +
geom_segment(data=swir_plt, mapping=aes(x=x, xend=xend, 
                                        y=swir_w, yend=swir_w, 
                                        color="SWIR"), size=3) +
scale_color_manual(name="", values="red") +
ggtitle(i)
png(filename=paste0("~/dropbox/owens/sfwcrft/transect-swir compare/output/",
                    i, "_plot.png"), width=8, height=6, units="in", res=300)
print(p2)
dev.off()
write.csv(swir_sum, 
          file=paste0("~/dropbox/owens/sfwcrft/transect-swir compare/output/",
                      i, "_swir.csv"))
write.csv(paths_sum, 
          file=paste0("~/dropbox/owens/sfwcrft/transect-swir compare/output/",
                      i, "_transect.csv"))
}

