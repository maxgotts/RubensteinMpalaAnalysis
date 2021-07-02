df <- read.csv("/Users/maxgotts/Desktop/MPALA/Whitesheets/ConvertedWhitesheets.csv")

library(ggplot2)
(l.ndvi <- lm(Total.animals ~ NDVI, df))
summary(l.ndvi)

(l.evi <- lm(Total.animals ~ EVI, df))
summary(l.evi)

ggplot(df, aes(x=Total.animals))+
  geom_point(y=df$EVI)+
  geom_point(y=df$NDVI)
