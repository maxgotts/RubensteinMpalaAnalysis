rm(list=ls())

library(dplyr)
library(lubridate)
library(ggplot2)

source('~/Desktop/MPALA/mpala.R')

df <- filter(df, is.before(mdy(df$Date),ymd("2021-07-05")), !(Identifier%in%blacklist)) #Species%in%zebra.abbr
df <- duplicate.rows(df)

df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)

mean.lat <- mean(filter(df, is.before(mdy(df$Date),ymd("2021-06-14")))$Latitude)
mean.long <- mean(filter(df, is.before(mdy(df$Date),ymd("2021-06-14")))$Longitude)

df$Distance.from.origin <- sqrt((df$Longitude-mean.long)^2+(df$Latitude-mean.lat)^2)
df$Angle.from.origin <- atan((df$Latitude-mean.lat)/(df$Longitude-mean.long+1e-12))

df.before <- filter(df, is.before(mdy(df$Date),ymd("2021-06-14")))
df.while <- filter(df, is.strictly.after(mdy(df$Date),ymd("2021-06-14")), is.strictly.before(mdy(df$Date),ymd("2021-06-19")))
df.after <- filter(df, is.after(mdy(df$Date),ymd("2021-06-19")))

df.before$Exp.1 <- "before"
df.while$Exp.1 <- "while"
df.after$Exp.1 <- "after"

df.exp.1 <- rbind(df.before,df.while,df.after)

df.exp.1$Angle.from.origin <- as.numeric(df.exp.1$Angle.from.origin)
df.exp.1$Distance.from.origin <- as.numeric(df.exp.1$Distance.from.origin)
df.exp.1$Distance.to.water <- as.numeric(df.exp.1$Distance.to.water)
df.exp.1$Distance.secondary <- as.numeric(df.exp.1$Distance.secondary)
df.exp.1$Distance.tertiary <- as.numeric(df.exp.1$Distance.tertiary)
df.exp.1$NDVI <- as.numeric(df.exp.1$NDVI)
df.exp.1$EVI <- as.numeric(df.exp.1$EVI)
df.exp.1$Distance.to.mob <- as.numeric(df.exp.1$Distance.to.mob)


GROUP_ITEMS <- function(VARNAME) { return(list(valid=T,order=c("before","while","after"), limits=c("Before phalanx arrival","During phalanx invasion","After phalanx dispersion"), group_name="Phase of experiment", variable_name=VARNAME)) }
Anova(df.exp.1, "Angle.from.origin", "Exp.1",GROUP_ITEMS("Angle from original position"))
Anova(df.exp.1, "Distance.from.origin", "Exp.1",GROUP_ITEMS("Distance from original position"))
Anova(df.exp.1, "Distance.to.water", "Exp.1",GROUP_ITEMS("Distance from water"))
Anova(df.exp.1, "NDVI", "Exp.1",GROUP_ITEMS("NDVI"))
Anova(df.exp.1, "EVI", "Exp.1",GROUP_ITEMS("EVI"))
Anova(filter(df.exp.1,!is.na(Distance.to.mob)), "Distance.to.mob", "Exp.1",GROUP_ITEMS("Distance to closest mob"))

df.exp.1$Tree.cover <- find_replace(df.exp.1$Primary.habitat,data.frame(x=bushland,y=c(0.1,0.5,0.9)))
Anova(df.exp.1, "Tree.cover", "Exp.1",GROUP_ITEMS("Primary habitat tree cover"))
df.exp.1$Secondary.tree.cover <- find_replace(df.exp.1$Secondary.habitat,data.frame(x=bushland,y=c(0.1,0.5,0.9)))
Anova(df.exp.1, "Secondary.tree.cover", "Exp.1",GROUP_ITEMS("Secondary habitat tree cover"))
Anova(df.exp.1, "Distance.secondary", "Exp.1",GROUP_ITEMS("Distance to secondary habitat"))
df.exp.1$Tertiary.tree.cover <- find_replace(df.exp.1$Tertiary.habitat,data.frame(x=bushland,y=c(0.1,0.5,0.9)))
Anova(df.exp.1, "Tertiary.tree.cover", "Exp.1",GROUP_ITEMS("Tertiary habitat tree cover"))
Anova(df.exp.1, "Distance.tertiary", "Exp.1",GROUP_ITEMS("Distance to tertiary habitat"))


