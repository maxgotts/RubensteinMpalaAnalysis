#---------------------------------------------------------------------------#
#---------------------Distance to water by hour-----------------------------#
#---------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_2/DistanceWater_DistanceCattle_setup.R')
df$Species <- factor(df$Species, levels=c("GZ","PZ"))
levels(df$Species) <- c("Grevy's zebras","Plains zebras")
ggplot(filter(df,
         Distance.to.water<=0.1,
         Hour<=13,
         Hour>=6,
         Species%in%zebra.names,
         is.before(mdy(Date),ymd("2021-06-10"))),
  aes(
    x=Hour,
    y=Distance.to.water,
    color=Species
  ))+
  geom_point()+
  theme_bw()+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  theme(legend.position="none")+
  labs(x="Hour (24 hour time)", y="Distance to water (degrees)")+
  scale_color_manual(values=c(gz.2,pz.2))+
  facet_wrap(~Species)


#---------------------------------------------------------------------------#
#----------------------------Group size by hour-----------------------------#
#---------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_2/DistanceWater_DistanceCattle_setup.R')
df$Species <- factor(df$Species, levels=c("GZ","PZ"))
levels(df$Species) <- c("Grevy's zebras","Plains zebras")
ggplot(filter(df,
         Distance.to.water<=0.1,
         Hour<=13,
         Hour>=6,
         Species%in%zebra.names,
         is.before(mdy(Date),ymd("2021-06-10"))),
  aes(
    x=Hour,
    y=Total.animals,
    color=Species
  ))+
  geom_point()+
  theme_bw()+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  theme(legend.position="none")+
  labs(x="Hour (24 hour time)", y="Dazzle size (individuals)")+
  scale_color_manual(values=c(gz.2,pz.2))+
  facet_wrap(~Species)
#---------------------------------------------------------------------------#
ggplot(filter(df,
              Distance.to.water<=0.1,
              Hour<=13,
              Hour>=6,
              Species%in%zebra.names,
              is.strictly.after(mdy(Date),ymd("2021-06-10"))),
       aes(
         x=Hour,
         y=Total.animals,
         color=Species
       ))+
  geom_point()+
  theme_bw()+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  theme(legend.position="none")+
  labs(x="Hour (24 hour time)", y="Dazzle size (individuals)")+
  scale_color_manual(values=c(gz.2,pz.2))+
  facet_wrap(~Species)



#---------------------------------------------------------------------------#
#---------------------Water vs mob distance by species----------------------#
#---------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_2/DistanceWater_DistanceCattle_setup.R')
df$Species <- factor(df$Species, levels=c("GZ","PZ"))
levels(df$Species) <- c("Grevy's zebras","Plains zebras")
ggplot(
  filter(df,
         Distance.to.water<=0.1,
         Distance.from.mob<=2000,
         Species%in%zebra.names,
         !is.na(Mob.size.categorical)),
  aes(
    x=Distance.from.mob,
    y=Distance.to.water,
    color=Species #Hour
  ))+
  geom_point()+
  theme_bw()+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  labs(x="Distance from closest mob (m)",y="Distance to water (degrees)")+
  facet_wrap(~Species)+
  scale_color_manual(values=c(gz.2,pz.2))+
  theme(legend.position="none")



#---------------------------------------------------------------------------#
#---------------------Water vs mob distance by size of mob------------------#
#---------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_2/DistanceWater_DistanceCattle_setup.R')
df$Species <- factor(df$Species, levels=c("GZ","PZ"))
df$Mob.size.categorical <- factor(df$Mob.size.categorical, levels=c("Small","Medium","Large"))
levels(df$Mob.size.categorical) <- c("Small mob","Medium mob","Large mob")
ggplot(filter(df,
         Distance.to.water<=0.1,
         Distance.from.mob<=2000,
         Species%in%zebra.abbr,
         !is.na(Mob.size.categorical)),
  aes(
    x=Distance.from.mob,
    y=Distance.to.water,
    color=Species
  ))+  #Hour #Mob.size.categorical #Species
  geom_point()+
  theme_bw()+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  labs(x="Distance from closest mob (m)",y="Distance to water (degrees)")+
  facet_wrap(~Mob.size.categorical)+ #Mob.size.categorical #Species
  scale_color_manual(name="Species",labels=c("Grevy's zebra","Plains zebra"),values=c(gz.2,pz.2))+
  theme(legend.position="top")



#---------------------------------------------------------------------------#
#---------------Vegetation index by water distance--------------------------#
#---------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_2/DistanceWater_DistanceCattle_setup.R')
library(ggpmisc)
ggplot(filter(df,
               Distance.to.water<=0.1,
               Species%in%zebra.abbr,
               !is.na(Mob.size.categorical)),
       aes(x=Distance.to.water))+
  geom_point(aes(y=NDVI), color=ndvi.col)+#"yellow")+
  stat_smooth(method="lm",formula=y~poly(x,1), aes(y=NDVI), color=ndvi.col)+#"yellow")+
  stat_poly_eq(formula=y~poly(x,1), 
               aes(y=NDVI,label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color=ndvi.col,parse=TRUE,label.x="left")+
  geom_text(x=0.01,y=mean(df$NDVI)+0.01,label="NDVI", color=ndvi.col)+
  geom_point(aes(y=EVI), color=evi.col)+#"green")+
  stat_smooth(method="lm",formula=y~poly(x,1), aes(y=EVI), color=evi.col)+#"green")+
  stat_poly_eq(formula=y~poly(x,1), 
               aes(y=EVI,label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color=evi.col, parse=TRUE,label.x="right")+
  geom_text(x=0.01,y=mean(df$EVI)+0.01,label="EVI", color=evi.col)+
  theme_bw()+
  labs(x="Distance to water (degrees)",y="Vegetation Index")+
  scale_color_manual(name="Species",labels=c("NDVI","EVI"),values=c(gz.2,pz.2))+
  theme(legend.position="top")










