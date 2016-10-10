rm(list=ls())
load_all()
library(tidyverse)
library(ggplot2)

df1 <- read.csv("./data/water usage data.csv")
df2 <- gather(df1, area, water, T10.45:T29.45) %>%
  separate(area, into=c("dca", "trgtwet"), sep="\\.")
df2$Month <- ordered(df2$Month, levels=c("Nov", "Dec", "Jan", "Feb", "Mar", 
                                         "Apr", "May", "Jun"))
df2$trgtwet <- paste0(df2$trgtwet, "%")
df2$dca <- factor(df2$dca, levels=c("T10", "T26", "T29"), 
                  labels=c("T10-1b", "T26", "T29-2ab"))
p1 <- ggplot(df2, aes(x=Month, y=water)) +
  geom_point(aes(color=trgtwet)) +
  geom_path(aes(group=trgtwet, color=trgtwet)) +
  facet_grid(dca ~ .) +
  ylab("Water Usage (in/acre)") +
  scale_colour_brewer("Target Wetness", palette="Set1")
png(filename="~/dropbox/owens/sfwcrft/code_output/water_usage_plot.png", 
    height=6, width=6, units="in", res=300)
p1
dev.off()

df3 <- df2 %>% spread(Month, water)
write.csv(df3, file="~/dropbox/owens/sfwcrft/code_output/water_usage.csv", 
          row.names=FALSE)
