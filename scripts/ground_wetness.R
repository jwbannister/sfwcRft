load_all()
load("~/code/owensMaps/map_data.RData")
load_all("~/code/owensData")
library(tidyverse)

df1 <- query_AGOL("SFWCT", "20150528_SFWCT_Wetness_Collection", 0)
df1$monitorigng.date <- convert_ESRI_date(df1$monitorigng.date)
df2 <- filter(df1, monitorigng.date > "2016-11-13", 
              !(transition %in% c("Crust", "nCrust"))) %>%
    select(date=monitorigng.date, monitoring.person, transition, x, y)
df2$monitoring.person <- sapply(df2$monitoring.person, 
                                function(x) ifelse(x==" S", "SR", x))
write.csv(df2, file="~/Desktop/111416_points.csv")
# QA ground points in QGIS
df3 <- read.csv(file="~/Desktop/QA ground points.csv") %>%
    select(x, y, dt=date, transition)
df3$date <- as.POSIXct(strptime(df3$dt, "%Y-%m-%d %H:%M:%S"))
df3$trans.from <- substr(df3$transition, 1, 1)
df3$trans.to <- substr(df3$transition, 4, 4)

