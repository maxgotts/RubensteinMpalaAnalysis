#---------------------------------------------------------------------------------------------#
#-----------------------------NDVI by day controlled------------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
df <- filter(df, Loop%in%1:2)
vi <- get_vi_l12()

vi$ID <- paste0(vi$Latitude,":",vi$Longitude)
vi.sample <- melt(vi %>% dplyr::select(3,5,7,9,11) %>% sample_n(1000))
colnames(vi.sample) <- c("ID","Date","NDVI")
vi.sample$Date <- ymd(paste0("2021-",gsub("_","-",substr(vi.sample$Date,6,10))))
vi.sample$Days <- days_since_start(vi.sample$Date)
vi.sample$Species <- "Random"

df.ndvi <- df %>% filter(QuickSpecies %in% c(zebra.abbr, "Cattle")) %>% dplyr::select("Identifier","Date","Days","NDVI","QuickSpecies")
df.ndvi$Date <- mdy(df.ndvi$Date)
colnames(df.ndvi)[1] <- "ID"
colnames(df.ndvi)[5] <- "Species"

ndvi <- rbind(df.ndvi, vi.sample)

# ggplot(ndvi, aes(x=Days,y=NDVI, color=Species, shape=Species, fill=Species))+
#   geom_point(size=3)+
#   stat_smooth(method="lm",formula=y~x,fullrange=T)+
#   stat_poly_eq(formula=y~x,
#                aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
#                parse=TRUE,label.x="right",label.y="top")+
#   # stat_smooth(method="lm",formula=y~poly(x,2),fullrange=T)+
#   # stat_smooth(method="loess",formula=y~x)+
#   scale_color_manual(breaks=c(zebra.abbr,"Cattle","Random"),labels=c(zebra.names,"Cattle","Random"),values=c(gz.2,pz.2,cattle,"#000000"))+
#   scale_fill_manual(breaks=c(zebra.abbr,"Cattle","Random"),labels=c(zebra.names,"Cattle","Random"),values=c(gz.2,pz.2,cattle,"#000000"))+
#   scale_shape_manual(breaks=c(zebra.abbr,"Cattle","Random"),labels=c(zebra.names,"Cattle","Random"),values=c(16,17,15,23))+
#   theme_classic()+
#   labs(y="NDVI",x="Days since start of experiment",fill="Source",color="Source",shape="Source")+
#   
#   theme_classic()+theme(
#     legend.position = "top"
#   )


ggplot(ndvi, aes(x=Days,y=NDVI, color=Species, shape=Species, fill=Species))+
  geom_point(data=ndvi %>% filter(Species != "Random"), size=3)+
  stat_smooth(method="lm",formula=y~x,fullrange=T)+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               parse=TRUE,label.x="right",label.y="top")+
  # stat_smooth(method="lm",formula=y~poly(x,2),fullrange=T)+
  # stat_smooth(method="loess",formula=y~x)+
  scale_color_manual(breaks=c(zebra.abbr,"Cattle","Random"),labels=c(zebra.names,"Cattle","Random"),values=c(gz.2,pz.2,cattle,"#000000"))+
  scale_fill_manual(breaks=c(zebra.abbr,"Cattle","Random"),labels=c(zebra.names,"Cattle","Random"),values=c(gz.2,pz.2,cattle,"#000000"))+
  scale_shape_manual(breaks=c(zebra.abbr,"Cattle","Random"),labels=c(zebra.names,"Cattle","Random"),values=c(16,17,15,23))+
  theme_classic()+
  # lims(x=c(0,max(ss$Days)))+
  labs(y="NDVI",x="Days since start of experiment",fill="Source",color="Source",shape="Source")+
  
  theme_classic()+theme(
    legend.position = "top"
  )




#---------------------------------------------------------------------------------------------#
#------------------------------------Total cows by day----------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
ss.total.cattle <- ss.zebra %>% filter(Species=="GZ") # doesn't matter either way
ss.total.cattle$Species="Cattle"
ggplot(ss.total.cattle, aes(x=Days,y=Total.cattle, color=Species, fill=Species, shape=Species))+
  geom_point(size=2)+
  
  stat_smooth(method="lm",formula=y~x)+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c("Cattle"),labels=c("Cattle"),values=c(cattle))+
  scale_fill_manual(breaks=c("Cattle"),labels=c("Cattle"),values=c(cattle))+
  scale_shape_manual(breaks=c("Cattle"),labels=c("Cattle"),values=c(15))+
  
  theme_classic()+
  labs(y="Total cattle",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "none"
  )



#---------------------------------------------------------------------------------------------#
#--------------------------------Cattle density by day----------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
ggplot(df.zebra, aes(x=Days, y=Cattle.density, color=Species, shape=Species, fill=Species))+
  geom_point(size=2, data=ss.zebra, aes(y=(Med.cattle.density)))+geom_line(data=ss.zebra, aes(y=Med.cattle.density),linetype="dashed")+
  geom_point(size=2, fill="white", aes(shape=paste0(Species,1)))+
  
  geom_ribbon(data=ss.zebra, aes(y=0, ymin=Min.cattle.density, ymax=Max.cattle.density, fill=Species),alpha=0.2)+
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="Cattle density (within 2 km of herd)",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)



#---------------------------------------------------------------------------------------------#
#-----------------------------Distance from cattle by day-------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
ss.zebra.here <- ss.zebra
ggplot(df.zebra, aes(x=Days, y=Distance.from.mob, color=Species, shape=Species, fill=Species))+
  geom_point(size=2, data=ss.zebra.here, aes(y=(Med.distance.from.mob)))+geom_line(data=ss.zebra, aes(y=Med.distance.from.mob),linetype="dashed")+
  geom_point(size=2, fill="white", aes(shape=paste0(Species,1)))+
  
  geom_ribbon(data=ss.zebra.here, aes(y=0, ymin=Min.distance.from.mob, ymax=Max.distance.from.mob, fill=Species),alpha=0.2)+
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="Distance from closest cattle herd (m)",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)



#---------------------------------------------------------------------------------------------#
#------------------------Independence of bottom-up/sideways-----------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
ggplot(df.zebra, aes(x=Distance.from.mob, y=X.Green, color=Species, shape=Species, fill=Species))+
  geom_point(size=3)+
  
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="right",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,17,1,2))+
  
  theme_classic()+
  labs(x="Distance from nearest cattle herd (m)",y="% Green",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


ggplot(df.zebra, aes(x=Distance.from.mob, y=X.Cover, color=Species, shape=Species, fill=Species))+
  geom_point(size=3)+
  
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="right",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,17,1,2))+
  
  theme_classic()+
  labs(x="Distance from nearest cattle herd (m)",y="% Cover",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)

#---------------------------------------------------------------------------------------------#
#--------------------------Herd numbers and total animals by day------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
ggplot(ss.zebra, aes(x=Days,y=Total.sitings, color=Species, shape=Species, fill=Species))+
  geom_point(size=2)+
  
  stat_smooth(method="lm",formula=y~x)+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,17))+
  
  theme_classic()+
  labs(y="Total zebras",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


ggplot(ss.zebra, aes(x=Days,y=Total.herds, color=Species, shape=Species, fill=Species))+
  geom_point(size=2)+
  
  stat_smooth(method="lm",formula=y~x)+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,17))+
  
  theme_classic()+
  labs(y="Total zebra herds",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


#---------------------------------------------------------------------------------------------#
#--------------------------------------Herd size by day---------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
ggplot(df.zebra, aes(x=Days, y=Total.animals, color=Species, shape=Species, fill=Species))+
  geom_point(size=2, data=ss.zebra, aes(y=(Med.herd.size)))+geom_line(data=ss.zebra, aes(y=Med.herd.size),linetype="dashed")+
  geom_point(size=2,fill="white", aes(shape=paste0(Species,1)))+
  
  geom_ribbon(data=ss.zebra, aes(y=0, ymin=Min.herd.size, ymax=Max.herd.size, fill=Species),alpha=0.2)+
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="Herd size",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)

ggplot(ss.zebra, aes(x=Days, y=Max.herd.size, color=Species, shape=Species, fill=Species))+
  geom_point(size=2, aes(y=(Med.herd.size)))+geom_line(aes(y=Med.herd.size),linetype="dashed")+
  geom_point(size=2,fill="white", data=df.zebra, aes(x=Days,y=Total.animals, shape=paste0(Species,1)))+
  
  geom_ribbon(aes(ymin=Min.herd.size, ymax=Max.herd.size, fill=Species),alpha=0.2)+
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="Herd size",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)





#---------------------------------------------------------------------------------------------#
#------------------------------Herd size by cattle density------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
####
# the functions from summarystatistics required
zz <- as.data.frame(matrix(NA,nrow=0,ncol=1))
zz$V1 <- NULL
df$SplitVariable <- df$Cattle.density
df$SplitVariable <- floor((df$SplitVariable+1)/3)
df.split.cd <- split(df, df$SplitVariable)
for (species in zebra.abbr) {
  for (cd in names(df.split.cd)) {
    df.cd <- df.split.cd[[cd]] %>% filter(QuickSpecies==species)
    zz[nrow(zz)+1,"Cattle.density"] <- mean(df.cd$Cattle.density, na.rm=T)
    zz[nrow(zz),all_names("herd.size")] <- all_weighted(df.cd,"Total.animals",F)
    zz[nrow(zz),"Species"] <- species
    if (is.infinite(zz[nrow(zz),"Max.herd.size"]) | is.infinite(zz[nrow(zz),"Min.herd.size"])) zz[nrow(zz),] <- NA
  }
}
zz <- filter(zz,!is.na(Species))


ggplot(df.zebra, aes(x=Cattle.density, y=Total.animals, color=Species, shape=Species, fill=Species))+
  geom_point(size=2, data=zz, aes(y=(Med.herd.size)))+geom_line(data=zz, aes(y=Med.herd.size),linetype="dashed")+
  geom_point(size=2, fill="white", aes(shape=paste0(Species,1)))+
  
  geom_ribbon(data=zz, aes(y=0, ymin=Min.herd.size, ymax=Max.herd.size, fill=Species),alpha=0.2)+
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="Herd size",x="Cattle density (within 2 km of herd)",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


#---------------------------------------------------------------------------------------------#
#------------------------------Herd size by cattle density------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
####
# the functions from summarystatistics required
zz <- as.data.frame(matrix(NA,nrow=0,ncol=1))
zz$V1 <- NULL
df$SplitVariable <- df$Cattle.density
df$SplitVariable <- floor((df$SplitVariable+1)/5)
df.split.cd <- split(df, df$SplitVariable)
for (species in zebra.abbr) {
  for (cd in names(df.split.cd)) {
    df.cd <- df.split.cd[[cd]] %>% filter(QuickSpecies==species)
    zz[nrow(zz)+1,"Cattle.density"] <- mean(df.cd$Cattle.density, na.rm=T)
    zz[nrow(zz),all_names("distance.from.herd")] <- all_weighted(df.cd,"Distance.from.herd",F)
    zz[nrow(zz),"Species"] <- species
  }
}
zz <- filter(zz,!is.na(Species))
ggplot(df.zebra, aes(x=Cattle.density, y=Distance.from.herd, color=Species, shape=Species, fill=Species))+
  geom_point(size=2, data=zz, aes(y=(Med.distance.from.herd)))+geom_line(data=zz, aes(y=Med.distance.from.herd),linetype="dashed")+
  geom_point(size=2, fill="white", aes(y=Distance.from.herd, shape=paste0(Species,1)))+
  
  geom_ribbon(data=zz, aes(y=0, ymin=Min.distance.from.herd, ymax=Max.distance.from.herd, fill=Species),alpha=0.2)+
  
  stat_smooth(method="lm",formula=y~x)+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="Inter-herd distance (m)",x="Cattle density (within 2 km of herd)",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)



ggplot(zz, aes(x=Cattle.density, y=Max.distance.from.herd, color=Species, shape=Species, fill=Species))+
  # geom_point(size=2, aes(y=(Mean.distance.from.herd)))+geom_errorbar(aes(y=0, ymin=Mean.distance.from.herd-Sd.distance.from.herd, ymax=Mean.distance.from.herd+Sd.distance.from.herd), width=3)+
  geom_point(size=2, aes(y=(Med.distance.from.herd)))+geom_line(aes(y=Med.distance.from.herd),linetype="dashed")+
  geom_point(size=2, fill="white", data=df.zebra, aes(y=Distance.from.herd, shape=paste0(Species,1)))+
  
  geom_ribbon(aes(ymin=Min.distance.from.herd, ymax=Max.distance.from.herd, fill=Species),alpha=0.2)+
  
  stat_smooth(method="lm",formula=y~x)+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="Inter-herd distance (m)",x="Cattle density (within 2 km of herd)",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)



#---------------------------------------------------------------------------------------------#
#------------------------------Distance to herd by day----------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
ggplot(df.zebra, aes(x=Days, y=Distance.from.herd, color=Species, shape=Species, fill=Species))+
  geom_point(size=2, data=ss.zebra, aes(y=(Med.distance.from.herd)))+geom_line(data=ss.zebra, aes(y=Med.distance.from.herd),linetype="dashed")+
  geom_point(size=2, fill="white", aes(shape=paste0(Species,1)))+
  
  geom_ribbon(data=ss.zebra, aes(y=0, ymin=Min.distance.from.herd, ymax=Max.distance.from.herd, fill=Species),alpha=0.2)+
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="Inter-herd distance (m)",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


ggplot(df.zebra, aes(x=Cattle.density, y=Distance.from.herd, color=Species, shape=Species, fill=Species))+
  geom_point(size=2, data=ss.zebra, aes(x=Med.cattle.density, y=Med.distance.from.herd))+geom_line(data=ss.zebra, aes(x=Med.cattle.density, y=Med.distance.from.herd),linetype="dashed")+
  geom_point(size=2, fill="white", aes(shape=paste0(Species,1)))+
  
  geom_ribbon(data=ss.zebra, aes(y=0, x=Med.cattle.density, ymin=Min.distance.from.herd, ymax=Max.distance.from.herd, fill=Species),alpha=0.2)+
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="Inter-herd distance (m)",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)





#---------------------------------------------------------------------------------------------#
#------------------------------------% Grazing by day-----------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
ggplot(ss.zebra, aes(x=Days,y=X.Grazing, color=Species, shape=Species, fill=Species))+
  geom_point(size=2)+
  
  stat_smooth(method="lm",formula=y~x)+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,17))+
  scale_y_continuous(labels=scales::percent)+
  
  theme_classic()+
  labs(y="% Grazing",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)




#---------------------------------------------------------------------------------------------#
#-------------------------------------Model analysis------------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')


ss.gz <- ss %>% filter(Species=="GZ")
ss.pz <- ss %>% filter(Species=="PZ")
df.gz <- df %>% filter(Species=="GZ")
df.pz <- df %>% filter(Species=="PZ")


AIC(lm(Max.herd.size ~ Mean.distance.to.water + Max.X.green + Mean.cattle.density, ss.pz))
AIC(lm(Max.herd.size ~ Max.X.green + Mean.cattle.density, ss.pz))
AIC(lm(Max.herd.size ~ Mean.distance.to.water + Mean.cattle.density, ss.pz))
AIC(lm(Max.herd.size ~ Mean.distance.to.water + Max.X.green, ss.pz)) #
AIC(lm(Max.herd.size ~ Mean.distance.to.water, ss.pz))
AIC(lm(Max.herd.size ~ Max.X.green, ss.pz)) #
AIC(lm(Max.herd.size ~ Mean.cattle.density, ss.pz))


AIC(lm(Max.distance.from.herd ~ Mean.distance.to.water + Max.X.green + Mean.cattle.density, ss.pz))
AIC(lm(Max.distance.from.herd ~ Max.X.green + Mean.cattle.density, ss.pz))
AIC(lm(Max.distance.from.herd ~ Mean.distance.to.water + Mean.cattle.density, ss.pz))
AIC(lm(Max.distance.from.herd ~ Mean.distance.to.water + Max.X.green, ss.pz)) #
AIC(lm(Max.distance.from.herd ~ Mean.distance.to.water, ss.pz))
AIC(lm(Max.distance.from.herd ~ Max.X.green, ss.pz)) #
AIC(lm(Max.distance.from.herd ~ Mean.cattle.density, ss.pz))

AIC(lm(Max.herd.size ~ Max.midday.distance.to.water + Max.X.green + Mean.cattle.density, ss.pz))
AIC(lm(Max.herd.size ~ Max.midday.distance.to.water + Mean.cattle.density, ss.pz))
AIC(lm(Max.herd.size ~ X.hoof + Mean.cattle.density, ss.pz))
AIC(lm(Max.herd.size ~ Max.midday.distance.to.water + X.hoof, ss.pz))
AIC(lm(Max.herd.size ~ Max.midday.distance.to.water, ss.pz))
AIC(lm(Max.herd.size ~ X.hoof, ss.pz))
AIC(lm(Max.herd.size ~ Mean.cattle.density, ss.pz))

summary(lm(Max.distance.from.herd ~ Mean.distance.to.water + X.hoof + Mean.cattle.density, ss.pz))


AIC(lm(Max.herd.size ~ Min.distance.to.water + Max.X.green + Max.distance.from.mob, ss.pz))
AIC(lm(Max.herd.size ~ Max.X.green + Max.distance.from.mob, ss.pz))
AIC(lm(Max.herd.size ~ Min.distance.to.water + Max.distance.from.mob, ss.pz))
AIC(lm(Max.herd.size ~ Min.distance.to.water + Max.X.green, ss.pz)) #
AIC(lm(Max.herd.size ~ Min.distance.to.water, ss.pz))
AIC(lm(Max.herd.size ~ Max.X.green, ss.pz)) #
AIC(lm(Max.herd.size ~ Max.distance.from.mob, ss.pz))




AIC(lm(Max.herd.size ~ Min.distance.to.water + Max.X.green + Max.distance.from.mob, ss.pz))
AIC(lm(Max.herd.size ~ Max.X.green + Max.distance.from.mob, ss.pz))
AIC(lm(Max.herd.size ~ Min.distance.to.water + Max.distance.from.mob, ss.pz))
AIC(lm(Max.herd.size ~ Min.distance.to.water + Max.X.green, ss.pz)) #
AIC(lm(Max.herd.size ~ Min.distance.to.water, ss.pz))
AIC(lm(Max.herd.size ~ Max.X.green, ss.pz)) #
AIC(lm(Max.herd.size ~ Max.distance.from.mob, ss.pz))




AIC(lm(Max.herd.size ~ Max.midday.distance.to.water + Max.X.green + Max.distance.from.mob, ss.pz))
AIC(lm(Max.herd.size ~ Max.X.green + Max.distance.from.mob, ss.pz))
AIC(lm(Max.herd.size ~ Max.midday.distance.to.water + Max.distance.from.mob, ss.pz))
AIC(lm(Max.herd.size ~ Max.midday.distance.to.water + Max.X.green, ss.pz)) #
AIC(lm(Max.herd.size ~ Max.midday.distance.to.water, ss.pz))
AIC(lm(Max.herd.size ~ Max.X.green, ss.pz)) #
AIC(lm(Max.herd.size ~ Max.distance.from.mob, ss.pz))
####
####


AIC(lm(Max.herd.size ~ Max.midday.distance.to.water + Max.X.green + Mean.cattle.density, ss.pz))
AIC(lm(Max.herd.size ~ Max.X.green + Mean.cattle.density, ss.pz))
AIC(lm(Max.herd.size ~ Max.midday.distance.to.water + Mean.cattle.density, ss.pz))
AIC(lm(Max.herd.size ~ Max.midday.distance.to.water + Max.X.green, ss.pz)) #
AIC(lm(Max.herd.size ~ Max.midday.distance.to.water, ss.pz))
AIC(lm(Max.herd.size ~ Max.X.green, ss.pz)) #
AIC(lm(Max.herd.size ~ Mean.cattle.density, ss.pz))
####
####



AIC(lm(Max.herd.size ~ Max.midday.distance.to.water + Max.X.green + Mean.cattle.density, ss.pz))
AIC(lm(Max.herd.size ~ Max.X.green + Mean.cattle.density, ss.pz))
AIC(lm(Max.herd.size ~ Max.midday.distance.to.water + Mean.cattle.density, ss.pz))
AIC(lm(Max.herd.size ~ Max.midday.distance.to.water + Max.X.green, ss.pz)) #
AIC(lm(Max.herd.size ~ Max.midday.distance.to.water, ss.pz))
AIC(lm(Max.herd.size ~ Max.X.green, ss.pz)) #
AIC(lm(Max.herd.size ~ Mean.cattle.density, ss.pz))
####
####


AIC(lm(Max.herd.size ~ Min.midday.distance.to.water + Mean.NDVI + Mean.cattle.density, ss.gz))
AIC(lm(Max.herd.size ~ Mean.NDVI + Mean.cattle.density, ss.gz))
AIC(lm(Max.herd.size ~ Min.midday.distance.to.water + Mean.cattle.density, ss.gz))
AIC(lm(Max.herd.size ~ Min.midday.distance.to.water + Mean.NDVI, ss.gz)) #
AIC(lm(Max.herd.size ~ Min.midday.distance.to.water, ss.gz))
AIC(lm(Max.herd.size ~ Mean.NDVI, ss.gz)) #
AIC(lm(Max.herd.size ~ Mean.cattle.density, ss.gz))
####
summary(lm(Max.herd.size ~ Mean.NDVI + Mean.cattle.density, ss.gz))
####


AIC(lm(Max.herd.size ~ Min.midday.distance.to.water + Mean.NDVI + Mean.cattle.density, ss.gz))
AIC(lm(Max.herd.size ~ Mean.NDVI + Mean.cattle.density, ss.gz))
AIC(lm(Max.herd.size ~ Min.midday.distance.to.water + Mean.cattle.density, ss.gz))
AIC(lm(Max.herd.size ~ Min.midday.distance.to.water + Mean.NDVI, ss.gz)) #
AIC(lm(Max.herd.size ~ Min.midday.distance.to.water, ss.gz))
AIC(lm(Max.herd.size ~ Mean.NDVI, ss.gz)) #
AIC(lm(Max.herd.size ~ Mean.cattle.density, ss.gz))
####
summary(lm(Max.herd.size ~ Mean.NDVI + Mean.cattle.density, ss.gz))
####


BIC(lm(Max.herd.size ~ Min.midday.distance.to.water + Mean.NDVI + Mean.cattle.density, ss.gz))
BIC(lm(Max.herd.size ~ Mean.NDVI + Mean.cattle.density, ss.gz))
BIC(lm(Max.herd.size ~ Min.midday.distance.to.water + Mean.cattle.density, ss.gz))
BIC(lm(Max.herd.size ~ Min.midday.distance.to.water + Mean.NDVI, ss.gz)) #
BIC(lm(Max.herd.size ~ Min.midday.distance.to.water, ss.gz))
BIC(lm(Max.herd.size ~ Mean.NDVI, ss.gz)) #
BIC(lm(Max.herd.size ~ Mean.cattle.density, ss.gz))
####
summary(lm(Max.herd.size ~ Mean.NDVI + Mean.cattle.density, ss.gz))
####



summary(lm(Max.herd.size ~ Min.distance.to.water + Max.X.green + Max.distance.from.mob, ss.pz))

summary(lm(Max.herd.size ~ Max.X.green, ss.pz)) #
summary(lm(Max.herd.size ~ Mean.distance.to.water + Max.X.green, ss.pz)) #

# Distance.to.water, X.Green, Cattle.density

AIC(lm(Max.herd.size ~ Mean.distance.to.water + Mean.NDVI + Mean.cattle.density, ss.gz)) #
AIC(lm(Max.herd.size ~ Mean.distance.to.water + Mean.NDVI + Mean.cattle.density + Max.distance.from.mob, ss.gz)) #
AIC(lm(Max.herd.size ~ Mean.NDVI + Mean.cattle.density, ss.gz))
AIC(lm(Max.herd.size ~ Mean.distance.to.water + Mean.cattle.density, ss.gz))
AIC(lm(Max.herd.size ~ Mean.distance.to.water + Mean.NDVI, ss.gz)) #
AIC(lm(Max.herd.size ~ Mean.distance.to.water, ss.gz))
AIC(lm(Max.herd.size ~ Mean.NDVI, ss.gz)) #
AIC(lm(Max.herd.size ~ Mean.cattle.density, ss.gz))



summary(lm(Max.herd.size ~ Mean.distance.to.water + Mean.NDVI + Mean.cattle.density, ss.gz)) #









library(fitnesslandscapes2)

AIC(lm(Max.herd.size ~ Min.X.green + Max.cattle.density, ss.gz))
AIC(lm(Max.herd.size ~ Min.X.green + Max.cattle.density + Min.X.green * Max.cattle.density, ss.gz))
AIC(lm(Max.herd.size ~ Min.X.green + Min.cattle.density, ss.gz))
AIC(lm(Max.herd.size ~ Min.X.green + Min.cattle.density + Mean.NDVI, ss.gz)) # WATCH OUT FOR P-HACKING
AIC(lm(Max.herd.size ~ Min.X.green + Min.cattle.density + X.hoof, ss.gz))
AIC(lm(Max.herd.size ~ Min.X.green + Min.cattle.density + X.OG, ss.gz))
AIC(lm(Max.herd.size ~ Min.X.green + Min.cattle.density + Max.X.green, ss.gz))
AIC(lm(Max.herd.size ~ Min.X.green + Min.cattle.density + Mean.X.green, ss.gz))
AIC(lm(Max.herd.size ~ Min.X.green + Min.cattle.density + Min.distance.from.mob, ss.gz))
AIC(lm(Max.herd.size ~ Min.X.green + Min.cattle.density + Min.distance.from.mob, ss.gz))
summary(lm(Max.herd.size ~ Min.X.green + Min.cattle.density, ss.gz))


AIC(lm(Max.distance.from.herd ~ Min.X.green + Max.cattle.density, ss.pz))
AIC(lm(Max.distance.from.herd ~ Min.X.green + Max.cattle.density + Min.X.green * Max.cattle.density , ss.pz))
AIC(lm(Max.distance.from.herd ~ Min.X.green + Max.cattle.density + Max.distance.from.mob, ss.pz))
AIC(lm(Max.distance.from.herd ~ Min.X.green + Max.cattle.density + Max.distance.from.mob + Min.NDVI, ss.pz))
AIC(lm(Max.distance.from.herd ~ Min.X.green + Max.cattle.density + Max.distance.from.mob, ss.pz))
AIC(lm(Max.distance.from.herd ~ Min.X.green + Max.cattle.density + Max.distance.from.mob + X.hoof, ss.pz))
AIC(lm(Max.distance.from.herd ~ Min.X.green + Max.cattle.density + Max.distance.from.mob + X.hoof + X.hock, ss.pz))
AIC(lm(Max.distance.from.herd ~ Min.X.green + Max.cattle.density + Max.distance.from.mob + X.hoof + X.belly, ss.pz))
AIC(lm(Max.distance.from.herd ~ Min.X.green + Max.cattle.density + Max.distance.from.mob + X.hoof + X.OG, ss.pz))
summary(lm(Max.distance.from.herd ~ Min.X.green + Max.cattle.density + Max.distance.from.mob + X.hoof, ss.pz))


# BIC(lm(Total.animals ~ X.Green + Cattle.density, df.gz))
# BIC(lm(Total.animals ~ X.Green + Cattle.density + X.Green * Cattle.density, df.gz))
# BIC(lm(Total.animals ~ X.Green + Cattle.density + NDVI, df.gz))
# BIC(lm(Total.animals ~ X.Cover + Cattle.density + Distance.from.mob, df.gz))
# BIC(lm(Total.animals ~ Cattle.density + Distance.from.mob + X.Green, df.gz))









#---------------------------------------------------------------------------------------------#
#------------------------------------Distance to water----------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
ggplot(ss.zebra, aes(x=Days, y=Min.distance.to.water, color=Species, shape=Species, fill=Species))+
  geom_point(size=2, aes(y=(Med.distance.to.water)))+geom_line(aes(y=Med.distance.to.water),linetype="dashed")+
  geom_point(size=2,fill="white", data=df.zebra, aes(x=Days,y=Distance.to.water, shape=paste0(Species,1)))+
  
  geom_ribbon(aes(ymin=Min.distance.to.water, ymax=Max.distance.to.water, fill=Species),alpha=0.2)+
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="right",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="Distance to water (m)",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)

ggplot(ss.zebra, aes(x=Days, y=Min.midday.distance.to.water, color=Species, shape=Species, fill=Species))+
  geom_point(size=2, aes(y=(Med.distance.to.water)))+geom_line(aes(y=Med.midday.distance.to.water),linetype="dashed")+
  geom_point(size=2,fill="white", data=df.zebra %>% filter(abs(Time-1200)<=100), aes(x=Days,y=Distance.to.water, shape=paste0(Species,1)))+
  
  geom_ribbon(aes(ymin=Min.midday.distance.to.water, ymax=Max.midday.distance.to.water, fill=Species),alpha=0.2)+
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="right",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="Distance to water (m)",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)

ggplot(df.zebra %>% filter(abs(Time-1200)<=100), aes(x=Days, y=Distance.to.water, color=Species, shape=Species, fill=Species))+
  geom_point(size=2, data=ss.zebra, aes(y=(Med.distance.to.water)))+geom_line(data=ss.zebra, aes(y=Med.distance.to.water),linetype="dashed")+
  geom_point(size=2, fill="white", aes(shape=paste0(Species,1)))+
  
  geom_ribbon(data=ss.zebra, aes(y=0, ymin=Min.distance.to.water, ymax=Max.distance.to.water, fill=Species),alpha=0.2)+
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="Distance to water (m)",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)




#---------------------------------------------------------------------------------------------#
#------------------------Distance to water and total animals by day---------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
ggplot(ss.zebra, aes(x=Days, y=Min.distance.to.water, color=Species, shape=Species, fill=Species))+
  geom_point(size=2, aes(y=(Med.distance.to.water)))+geom_line(aes(y=Med.distance.to.water),linetype="dashed")+
  geom_point(size=2,fill="white", data=df.zebra, aes(x=Days,y=Distance.to.water, shape=paste0(Species,1)))+
  
  geom_ribbon(aes(ymin=Min.distance.to.water, ymax=Max.distance.to.water, fill=Species),alpha=0.2)+
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="right",label.y="bottom")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="Distance to water (m)",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)




#---------------------------------------------------------------------------------------------#
#------------------------Distance to water and total animals by day---------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
library(fitnesslandscapes2)
par(mar=c(5,5,5,5))
TPS_landscape(df %>% filter(Species=="GZ"), z="Total.animals",x="Cattle.density",y="X.Green",x_name="Cattle density (within 2 km of herd)",y_name="% Green",z_name="Herd size",output="contour",Theta=175,Phi=25)
TPS_landscape(df %>% filter(Species=="PZ"), z="Distance.from.herd",x="Cattle.density",y="X.Green",x_name="Cattle density (within 2 km of herd)",y_name="% Green",z_name="Inter-herd distance (m)",output="contour",Theta=125,Phi=30)
ss %>% filter(Species=="PZ")



#---------------------------------------------------------------------------------------------#
#----------------------------------------ARCHIVE----------------------------------------------#
#---------------------------------------------------------------------------------------------#

summary(lm(Mean.herd.size ~ Mean.cattle.density + Mean.NDVI + Mean.distance.from.mob + Mean.distance.from.herd + X.hock + X.hock*Mean.NDVI, ss.gz))
summary(lm(Max.herd.size ~ Mean.cattle.density + Mean.NDVI + Mean.distance.from.mob + X.hock + X.hock*Mean.NDVI, ss.gz))

summary(lm(Mean.herd.size ~ X.OG + X.LB + Mean.X.cover + Mean.X.cover*Mean.distance.from.mob, ss.pz))
summary(lm(Mean.distance.from.herd ~ X.OG + X.LB + Mean.X.cover + Mean.distance.from.mob + Mean.NDVI + Mean.X.cover*Mean.distance.from.mob + Mean.NDVI*Mean.X.cover, ss.pz))
TPS_landscape(ss.gz,z="Mean.herd.size",x="X.OG",y="Mean.X.cover")
TPS_landscape(ss.gz,z="Mean.distance.from.herd",x="X.OG",y="Mean.X.cover")










