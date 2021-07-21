rm(list=ls())
source('~/Desktop/MPALA/mpala.R')
df <- get_df()
source('~/Desktop/MPALA/Analysis/Exp_1/Experiment_1.R')
df$Time <- paste(pad_zeros(df$Hour,2),pad_zeros(df$Minute,2),sep=":")
df$Group.composition <- find_replace(df$Multispecies,data.frame(x=c(NA,0,1),y=c("No zebra species","One zebra species","Two zebra species")))
df$Species.type <- df$QuickSpecies
df$Experiment_phase_3 <- df$Exp.1
df$Experiment_phase_2 <- df$Exp.1_2

df <- filter(df, !(Identifier%in%blacklist))

ordering <- c("Identifier","Date","Time","Loop","GPS.x","GPS.y","Latitude","Longitude",
              "Species","Species.type","Group.composition","Experiment_phase_3","Experiment_phase_2",
              "NDVI","EVI",
              "Primary.habitat","Secondary.habitat","Tertiary.habitat","Distance.secondary","Distance.tertiary",
              "Sun","Wind","Rain","Activity","Bush.type","Grass.height","Grass.color",
              "Distance.from.mob","Closest.mob.size","Distance.from.herd","Closest.herd.size",
              "Grass.spp.1","Grass.spp.2","Grass.spp.3","Grass.spp.4")

write.csv(df[,ordering], "/Users/maxgotts/Desktop/MPALA/Rubenstein_ConvertedWhitesheets.csv",row.names=F)


if (FALSE) {
  source('~/Desktop/MPALA/Analysis/DanFriendly.R')
}



