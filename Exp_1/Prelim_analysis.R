rm(list=ls())

library(dplyr)
library(lubridate)

source('~/Desktop/MPALA/mpala.R')

df <- filter(df, is.before(mdy(df$Date),ymd("2021-07-05")), Species%in%zebra.abbr)

mean.lat <- mean(filter(df, is.strictly.before(mdy(df$Date),ymd("2021-06-17")))$Latitude)
mean.long <- mean(filter(df, is.strictly.before(mdy(df$Date),ymd("2021-06-17")))$Longitude)

df$Distance.from.origin <- sqrt((df$Longitude-mean.long)^2+(df$Latitude-mean.lat)^2)
df$Angle.from.origin <- atan((df$Latitude-mean.lat)/(df$Longitude-mean.long+1e-12))

df.before <- filter(df, is.strictly.before(mdy(df$Date),ymd("2021-06-17")))
df.after <- filter(df, is.strictly.after(mdy(df$Date),ymd("2021-06-16")))

df.before$Exp.1 <- "before"
df.after$Exp.1 <- "after"

df.exp.1.temp <- rbind(df.before,df.after)
df.exp.1 <- matrix(nrow=1,ncol=ncol(df.exp.1.temp))#as.data.frame(matrix(nrow=nrow(df.exp.1.temp),ncol=ncol(df.exp.1.temp)))
for (dazzle in 1:nrow(df.exp.1.temp)) {
  for (i in 1:df.exp.1.temp[dazzle,"Total.animals"]) {
    df.exp.1 <- rbind(df.exp.1,as.matrix(df.exp.1.temp[dazzle,]))
  }
}
df.exp.1 <- filter(as.data.frame(df.exp.1), !is.na(Date))
rownames(df.exp.1) <- NULL

Anova(df.exp.1, "Angle.from.origin", "Exp.1")
Anova(df.exp.1, "Distance.from.origin", "Exp.1")
Anova(df.exp.1, "Distance.to.water", "Exp.1")
Anova(df.exp.1, "Distance.secondary", "Exp.1")


Anova(filter(df.exp.1, Species=="GZ"), "Angle.from.origin", "Exp.1")

df.exp.1$Tree.cover <- find_replace(df.exp.1$Primary.habitat,data.frame(x=bushland,y=c(0.1,0.5,0.9)))
Anova(df.exp.1, "Tree.cover", "Exp.1")


Anova(filter(df.exp.1,!is.na(Distance.to.mob)), "Distance.to.mob", "Exp.1")




#while stage