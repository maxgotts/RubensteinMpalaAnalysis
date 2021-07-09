rm(list=ls())

source('~/Desktop/MPALA/mpala.R')

df <- filter(df, is.before(mdy(df$Date),ymd("2021-07-05")), !(Identifier%in%blacklist)) #Species%in%zebra.abbr

df.before <- filter(df, is.before(mdy(df$Date),ymd("2021-06-14")))
df.while <- filter(df, is.strictly.after(mdy(df$Date),ymd("2021-06-14")), is.strictly.before(mdy(df$Date),ymd("2021-06-19")))
df.after <- filter(df, is.after(mdy(df$Date),ymd("2021-06-19")))

# df.before <- filter(df, is.strictly.before(mdy(df$Date),ymd("2021-06-18")))
# df.while <- data.frame(intro=1:10,QuickSpecies="")
# df.after <- filter(df, is.after(mdy(df$Date),ymd("2021-06-18")))

write.csv(filter(df.before, QuickSpecies=="GZ"),"/Users/maxgotts/Desktop/MPALA/Analysis/Habitat_2/Map/CSV/GZ_before.csv", row.names=FALSE)
write.csv(filter(df.before, QuickSpecies=="PZ"),"/Users/maxgotts/Desktop/MPALA/Analysis/Habitat_2/Map/CSV/PZ_before.csv", row.names=FALSE)
write.csv(filter(df.before, QuickSpecies=="Cattle"),"/Users/maxgotts/Desktop/MPALA/Analysis/Habitat_2/Map/CSV/Cattle_before.csv", row.names=FALSE)
write.csv(filter(df.while, QuickSpecies=="GZ"),"/Users/maxgotts/Desktop/MPALA/Analysis/Habitat_2/Map/CSV/PZ_while.csv", row.names=FALSE)
write.csv(filter(df.while, QuickSpecies=="PZ"),"/Users/maxgotts/Desktop/MPALA/Analysis/Habitat_2/Map/CSV/GZ_while.csv", row.names=FALSE)
write.csv(filter(df.while, QuickSpecies=="Cattle"),"/Users/maxgotts/Desktop/MPALA/Analysis/Habitat_2/Map/CSV/Cattle_while.csv", row.names=FALSE)
write.csv(filter(df.after, QuickSpecies=="GZ"),"/Users/maxgotts/Desktop/MPALA/Analysis/Habitat_2/Map/CSV/PZ_after.csv", row.names=FALSE)
write.csv(filter(df.after, QuickSpecies=="PZ"),"/Users/maxgotts/Desktop/MPALA/Analysis/Habitat_2/Map/CSV/GZ_after.csv", row.names=FALSE)
write.csv(filter(df.after, QuickSpecies=="Cattle"),"/Users/maxgotts/Desktop/MPALA/Analysis/Habitat_2/Map/CSV/Cattle_after.csv", row.names=FALSE)


cat("Writing...\n")
if (FALSE) {
  source('~/Desktop/MPALA/Analysis/Habitat_2/Map/GenerateMapCSV.R')
}