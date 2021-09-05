rm(list=ls())
source('~/Desktop/MPALA/mpala.R')
df <- get_df()
ss <- get_ss()
source('~/Desktop/MPALA/Analysis/Exp_1/Experiment_1.R')
# df <- filter(df, abs(Time-1200)<=100)
df$Time <- paste(pad_zeros(df$Hour,2),pad_zeros(df$Minute,2),sep=":")
df$Group.composition <- find_replace(df$Multispecies,data.frame(x=c(NA,0,1),y=c("No zebra species","One zebra species","Two zebra species")))
df$Species.type <- df$QuickSpecies
df$Experiment_phase_3 <- df$Exp.1
df$Experiment_phase_2 <- df$Exp.1_2
df$Habitat <- df$Primary.habitat

df <- filter(df, !(Identifier%in%blacklist), Species.type%in%zebra.abbr)

ordering <- c("Identifier","Date","Days","Time","Loop","GPS.x","GPS.y","Latitude","Longitude",
              "Species","Species.type","Group.composition","Experiment_phase_3","Experiment_phase_2",
              "Total.animals","NDVI","EVI","Random.NDVI","Distance.to.water",
              "Habitat",
              "Sun","Wind","Rain","Activity","Bush.type","Grass.height","Grass.color",
              "X.Green","X.Cover",
              "Distance.from.mob","Closest.mob.size","Distance.from.herd","Closest.herd.size",
              "Cattle.density",
              "Grass.spp.1","Grass.spp.2","Grass.spp.3","Grass.spp.4")

write.csv(df[,ordering], "/Users/maxgotts/Desktop/MPALA/Rubenstein_ConvertedWhitesheets.csv",row.names=F)
ss <- ss %>% filter(Species%in%zebra.abbr)
ss$X.Lh <- NULL
write.csv(ss, "/Users/maxgotts/Desktop/MPALA/Rubenstein_StatisticsWhitesheets.csv", row.names=F)

if (FALSE) {
  source('~/Desktop/MPALA/Analysis/DanFriendly.R')
}



