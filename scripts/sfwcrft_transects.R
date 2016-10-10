load_all("~/code/owensData")
library(tidyverse)
library(ggplot2)

query1 <- "SELECT * FROM sfwct.wetness_summary WHERE dcm='T26' AND day='2016-04-26'"
df1 <- query_owenslake(query1)
df2 <- filter(df1, trgtwet!=0, class!="D")
df2$class <- ordered(df2$class, levels=c("M", "W"), 
                     labels=c("Moist", "Wet"))
df2$cover <- df2$round * 100

label_df <- data.frame(trgtwet=c(45, 45, 55, 55, 65, 65, 75, 75), 
                       pos=c(2.5, 20.5, 5.5, 33, 6, 42, 12.5, 39), 
                       label=c("W = 5%", "M = 36%", "W = 11%", "M = 55%", 
                               "W = 12%", "M = 72%", "W = 25%", "M = 53%"))

swir_df <- data.frame(swir=c(24, 48, 69, 65), x=c(42, 52, 62, 72), 
                      xend=c(48, 58, 68, 78))

p1 <- df2[order(df2$class, decreasing=T), ] %>% 
    ggplot(aes(x=trgtwet, y=cover)) +
    geom_bar(stat="identity", aes(fill=class), color="black") +
    scale_x_continuous(name="Target Wetness", breaks=c(45, 55, 65, 75), 
                       labels=c("45%", "55%", "65%", "75%")) +
    scale_y_continuous(name="", breaks=c(0, 20, 40, 60, 80, 100), 
                       minor_breaks=seq(0, 100, 5), limits=c(0, 105), 
                       expand=c(0, 0), labels=c("0%", "20%", "40%", "60%", 
                                                "80%", "100%")) +
    scale_fill_manual(name="Ground", values=c("#33a02c", "#0000f6")) +
    geom_segment(data=swir_df, mapping=aes(x=x, xend=xend, y=swir, yend=swir, 
                                           color="SWIR"), size=3) +
    scale_color_manual(name="", values="red")
png(filename="~/Desktop/t26_Apr26_areas.png", width=8, height=6, units="in", 
    res=300)
p1
dev.off()

library(rgdal)
paths_df <- readOGR(path.expand("~/Desktop/presentation working files/temp/"), 
                    "trans_example")@data
path_lengths <- paths_df %>% group_by(buff) %>%
    summarize(length=sum(length)) %>% ungroup()
paths_sum <- paths_df %>% group_by(buff, class) %>%
    summarize(length=sum(length)) %>% ungroup() %>%
    left_join(path_lengths, by="buff") %>%
    mutate(cover=round(length.x/length.y, 2))
paths_cast <- paths_sum %>% select(-length.x, -length.y) %>%
    spread(class, cover)
paths_cast$MW <- paths_cast$M + paths_cast$W
names(paths_cast)[2:5] <- tolower(paste0("grnd_", names(paths_cast)[2:5]))
paths <- paths_cast %>% select(-grnd_d) %>%
    gather(class, cover, 2:4)
paths$cover <- paths$cover * 100
paths$class <- ordered(paths$class, levels=c("grnd_w", "grnd_m", "grnd_mw"), 
                     labels=c("Wet", "Moist", "MW"))

swir_paths <- data.frame(x=c(0.7, 1.7, 2.7, 3.7, 4.7, 5.7), 
                         xend=c(1.3, 2.3, 3.3, 4.3, 5.3, 6.3), 
                         swir_w=c(77, 50, 75, 60, 37, 12)) 

plot_df <- filter(paths, class!="MW")
p2 <- plot_df[order(plot_df$class, decreasing=F), ] %>% 
    ggplot(aes(x=buff, y=cover)) +
    geom_bar(stat="identity", aes(fill=class), color="black") +
    scale_x_continuous(name="Transect", breaks=seq(1, 6, 1)) +
    scale_y_continuous(name="", breaks=c(0, 20, 40, 60, 80, 100), 
                       minor_breaks=seq(0, 100, 5), limits=c(0, 105), 
                       expand=c(0, 0), labels=c("0%", "20%", "40%", "60%", 
                                                "80%", "100%")) +
    scale_fill_manual(name="Ground", values=c("#0000f6", "#33a02c")) +
    geom_segment(data=swir_paths, mapping=aes(x=x, xend=xend, 
                                              y=swir_w, yend=swir_w, 
                                           color="SWIR"), size=3) +
    scale_color_manual(name="", values="red")
png(filename="~/Desktop/t26_Apr26_paths", width=8, height=6, units="in", 
    res=300)
p2
dev.off()

