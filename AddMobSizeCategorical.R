df$Mob.size.categorical <- NA
first.third <- quantile(filter(df,QuickSpecies=="Cattle", !is.na(Total.animals))$Total.animals, 0.45)[[1]]
second.third <- quantile(filter(df,QuickSpecies=="Cattle", !is.na(Total.animals))$Total.animals, 0.66)[[1]]
df[!is.na(df$Closest.mob.size)&df$Closest.mob.size<=first.third, "Mob.size.categorical"] <- "Small"
df[!is.na(df$Closest.mob.size)&df$Closest.mob.size>first.third&df$Closest.mob.size<=second.third, "Mob.size.categorical"] <- "Medium"
df[!is.na(df$Closest.mob.size)&df$Closest.mob.size>second.third, "Mob.size.categorical"] <- "Large"

if (FALSE) {
  source('~/Desktop/MPALA/Analysis/AddMobSizeCategorical.R')
}