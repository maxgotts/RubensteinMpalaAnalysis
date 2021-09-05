source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')

#---------------------------------------------------------------------------------------------#
#---------------------------Are they compensating for decaying VI---------------------------#
#---------------------------------------------------------------------------------------------#

#-------------------------NDVI by day with random-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
df <- filter(df, Loop%in%1:2)
vi <- get_vi_l12()

vi$ID <- paste0(vi$Latitude,":",vi$Longitude)
vi.sample <- melt(vi %>% dplyr::select(3,5,7,9,11) %>% sample_n(1000))
colnames(vi.sample) <- c("ID","Date","NDVI")
vi.sample$Date <- ymd(paste0("2021-",gsub("_","-",substr(vi.sample$Date,6,10))))
vi.sample$Days <- days_since_start(vi.sample$Date)
vi.sample$Species <- "Random"

df.ndvi <- df %>% filter(QuickSpecies %in% c(zebra.abbr, "Cattle"), ) %>% dplyr::select("Identifier","Date","Days","NDVI","QuickSpecies")
df.ndvi$Date <- mdy(df.ndvi$Date)
colnames(df.ndvi)[1] <- "ID"
colnames(df.ndvi)[5] <- "Species"

ndvi <- rbind(df.ndvi, vi.sample)
ggplot(ndvi, aes(x=Days,y=NDVI, color=Species, shape=Species, fill=Species))+
  geom_point(size=3)+
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
  labs(y="NDVI",x="Days since start of experiment",fill="Source",color="Source",shape="Source")+
  
  theme_classic()+theme(
    legend.position = "top"
  )

#-------------------------NDVI by day with random FOR CATTLE-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
df <- filter(df, Loop%in%1:2)
vi <- get_vi_l12()

vi$ID <- paste0(vi$Latitude,":",vi$Longitude)
vi.sample <- melt(vi %>% dplyr::select(3,5,7,9,11) %>% sample_n(1000))
colnames(vi.sample) <- c("ID","Date","NDVI")
vi.sample$Date <- ymd(paste0("2021-",gsub("_","-",substr(vi.sample$Date,6,10))))
vi.sample$Days <- days_since_start(vi.sample$Date)
vi.sample$Species <- "Random"

df.ndvi <- df %>% filter(Species %in% c("MC","CC","CKC")) %>% dplyr::select("Identifier","Date","Days","NDVI","Species")
df.ndvi$Date <- mdy(df.ndvi$Date)
colnames(df.ndvi)[1] <- "ID"
colnames(df.ndvi)[5] <- "Species"

ndvi <- rbind(df.ndvi, vi.sample)
ggplot(ndvi, aes(x=Days,y=NDVI, color=Species, shape=Species, fill=Species))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~x,fullrange=T)+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               parse=TRUE,label.x="right",label.y="top")+
  # stat_smooth(method="lm",formula=y~poly(x,2),fullrange=T)+
  # stat_smooth(method="loess",formula=y~x)+
  scale_color_manual(breaks=c("MC","CC","CKC","Random"),labels=c("Mpala Cattle","Community Cattle","Community Kaparo Cattle","Random"),values=c(red,orange,yellow,"#000000"))+
  scale_fill_manual(breaks=c("MC","CC","CKC","Random"),labels=c("Mpala Cattle","Community Cattle","Community Kaparo Cattle","Random"),values=c(red,orange,yellow,"#000000"))+
  scale_shape_manual(breaks=c("MC","CC","CKC","Random"),labels=c("Mpala Cattle","Community Cattle","Community Kaparo Cattle","Random"),values=c(16,17,15,23))+
  theme_classic()+
  labs(y="NDVI",x="Days since start of experiment",fill="Source",color="Source",shape="Source")+
  
  theme_classic()+theme(
    legend.position = "top"
  )
  

#-------------------------NDVI by day controlled by random-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
df.ndvi <- df %>% filter(QuickSpecies %in% c(zebra.abbr, "Cattle")) %>% dplyr::select("Identifier","Date","Days","NDVI","QuickSpecies","Random.NDVI")
df.ndvi$Species <- df.ndvi$QuickSpecies
df.ndvi$NDVI <- (df.ndvi$NDVI-df.ndvi$Random.NDVI)/df.ndvi$Random.NDVI
ggplot(df.ndvi, aes(x=Days,y=NDVI, color=Species, shape=Species, fill=Species))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~x,fullrange=T)+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               parse=TRUE,label.x="right",label.y="bottom")+
  # stat_smooth(method="lm",formula=y~poly(x,2),fullrange=T)+
  # stat_smooth(method="loess",formula=y~x)+
  scale_color_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(gz.2,pz.2,cattle,"#000000"))+
  scale_fill_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(gz.2,pz.2,cattle,"#000000"))+
  scale_shape_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(16,17,15,23))+
  scale_y_continuous(labels=scales::percent)+
  
  theme_classic()+
  labs(y="% Advantage on NDVI compared to random points",x="Days since start of experiment",fill="Source",color="Source",shape="Source")+
  
  theme_classic()+theme(
    legend.position = "top"
  )

lm(NDVI ~ Days, filter(df, Species=="Cattle", is.after(mdy(Date), "2021-06-21"), !is.na(NDVI)))
lm(NDVI ~ Days, filter(df, Species=="Cattle", is.before(mdy(Date), "2021-06-20"), !is.na(NDVI)))
lm(NDVI ~ Days, filter(df, Species=="Cattle", !is.na(NDVI)))
summary(lm(NDVI ~ Days, filter(df, Species=="Cattle", !is.na(NDVI))))
shapiro.test(filter(df, Species=="Cattle", !is.na(NDVI))$NDVI)


#-------------------------EVI by day with random-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
vi <- get_vi_l12()

vi$ID <- paste0(vi$Latitude,":",vi$Longitude)
vi.sample <- melt(vi %>% dplyr::select(4,6,8,10,11) %>% sample_n(1000))
colnames(vi.sample) <- c("ID","Date","EVI")
vi.sample$Date <- ymd(paste0("2021-",gsub("_","-",substr(vi.sample$Date,6,10))))
vi.sample$Days <- days_since_start(vi.sample$Date)
vi.sample$Species <- "Random"

df.ndvi <- df %>% filter(QuickSpecies %in% c(zebra.abbr, "Cattle")) %>% dplyr::select("Identifier","Date","Days","EVI","QuickSpecies")
df.ndvi$Date <- mdy(df.ndvi$Date)
colnames(df.ndvi)[1] <- "ID"
colnames(df.ndvi)[5] <- "Species"

ndvi <- rbind(df.ndvi, vi.sample)

ggplot(ndvi, aes(x=Days,y=EVI, color=Species, shape=Species, fill=Species))+
  geom_point(size=3)+
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
  labs(y="EVI",x="Days since start of experiment",fill="Source",color="Source",shape="Source")+
  
  theme_classic()+theme(
    legend.position = "top"
  )



lm(NDVI ~ Days, filter(df, Species=="Cattle", is.after(mdy(Date), "2021-06-21"), !is.na(NDVI)))
lm(NDVI ~ Days, filter(df, Species=="Cattle", is.before(mdy(Date), "2021-06-20"), !is.na(NDVI)))
lm(NDVI ~ Days, filter(df, Species=="Cattle", !is.na(NDVI)))
summary(lm(NDVI ~ Days, filter(df, Species=="Cattle", !is.na(NDVI))))
shapiro.test(filter(df, Species=="Cattle", !is.na(NDVI))$NDVI)




#---------------------------------------------------------------------------------------------#
#-------------------------What is changing: herd density or herd size-------------------------#
#---------------------------------------------------------------------------------------------#

#-------------------------Herd size by day-------------------------#
# ggplot(df %>% filter(Species %in% zebra.abbr), aes(x=Days,y=Total.animals, color=Species, shape=Species, fill=Species))+
#   geom_point(size=3)+
#   stat_smooth(method="lm",formula=y~x,fullrange=T)+
# 
#   scale_color_manual(breaks=c(zebra.abbr),labels=c(zebra.names),values=c(gz.2,pz.2))+
#   scale_fill_manual(breaks=c(zebra.abbr),labels=c(zebra.names),values=c(gz.2,pz.2))+
#   scale_shape_manual(breaks=c(zebra.abbr),labels=c(zebra.names),values=c(16,17))+
# 
#   theme_classic()+
#   labs(y="Herd size",x="Days since start of experiment",fill="Species",color="Species",shape="Species")+
# 
#   theme_classic()+theme(
#     legend.position = "top"
#   )+
#   facet_wrap(~Species)


#-------------------------Herd size by day (means + raw)-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
ggplot(df %>% filter(Species %in% zebra.abbr), aes(x=Days,y=Total.animals, color=Species, shape=Species, fill=Species))+
  geom_point(size=2)+
  
  geom_point(size=3, fill="white", data=ss %>% filter(Species %in% zebra.abbr), aes(x=Days,y=Mean.herd.size, shape=paste0(Species,1)))+
  geom_errorbar(data=ss %>% filter(Species %in% zebra.abbr), aes(y=Mean.herd.size, ymin=Mean.herd.size-Sd.herd.size, ymax=Mean.herd.size+Sd.herd.size))+
  stat_smooth(method="lm",formula=y~x)+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(1,16,2,17))+
  
  theme_classic()+
  labs(y="Herd size",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


#-------------------------Herd size by day (min, med, + max)-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
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




#-------------------------Herd density by day (means + raw)-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
ggplot(df %>% filter(Species %in% zebra.abbr), aes(x=Days,y=Approximate.density, color=Species, shape=Species, fill=Species))+
  geom_point(size=2)+
  
  geom_point(size=3, fill="white", data=ss %>% filter(Species %in% zebra.abbr), aes(x=Days,y=Mean.density, shape=paste0(Species,1)))+
  geom_errorbar(data=ss %>% filter(Species %in% zebra.abbr), aes(y=Mean.density, ymin=Mean.density-Sd.density, ymax=Mean.density+Sd.density))+
  stat_smooth(method="lm",formula=y~x)+
  stat_poly_eq(formula=y~exp(x),
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(1,16,2,17))+
  
  theme_classic()+
  labs(y=bquote("Herd density (number of individuals within 1"~km~"standardized by area)"),x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)



#-------------------------Inter-herd distance by day (means + raw)-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
ggplot(df.zebra, aes(x=Days,y=Distance.from.herd, color=Species, shape=Species, fill=Species))+
  geom_point(size=2)+
  
  geom_point(size=3, fill="white", data=ss.zebra, aes(x=Days,y=Mean.distance.from.herd, shape=paste0(Species,1)))+
  geom_errorbar(data=ss.zebra, aes(y=Mean.distance.from.herd, ymin=Mean.distance.from.herd-Sd.distance.from.herd, ymax=Mean.distance.from.herd+Sd.distance.from.herd))+
  stat_smooth(method="lm",formula=y~x)+
  stat_poly_eq(formula=y~exp(x),
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(1,16,2,17))+
  
  theme_classic()+
  labs(y="Inter-herd distance (minimum)",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


#-------------------------Inter-herd distance by day (min, med, max)-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
# ggplot(ss.zebra, aes(x=Days, y=Max.distance.from.herd, color=Species, shape=Species, fill=Species))+
#   geom_point(size=2, aes(y=(Med.distance.from.herd)))+geom_line(aes(y=Med.distance.from.herd),linetype="dashed")+
#   geom_point(size=2,fill="white", data=df.zebra, aes(x=Days,y=Distance.from.herd, shape=paste0(Species,1)))+
#   
#   geom_ribbon(aes(ymin=Min.distance.from.herd, ymax=Max.distance.from.herd, fill=Species),alpha=0.2)+
#   # stat_smooth(formula=y~x, method="lm")+
#   # stat_poly_eq(formula=y~x,
#                # aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
#                # color="black",parse=TRUE,label.x="left",label.y="top")+
#   
#   scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
#   scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
#   scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
#   
#   theme_classic()+
#   labs(y="Inter-herd distance (m)",x="Experiment day",fill="Species",color="Species",shape="Species")+
#   
#   theme_classic()+theme(
#     legend.position = "top"
#   )+
#   facet_wrap(~Species)

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


# #-------------------------Inter-herd distance (opposite species) by day (min, med, max)-------------------------#
# ggplot(ss.zebra, aes(x=Days, y=Max.distance.from.opp.herd, color=Species, shape=Species, fill=Species))+
#   geom_point(size=2, aes(y=(Med.distance.from.opp.herd)))+geom_line(aes(y=Med.distance.from.opp.herd),linetype="dashed")+
#   geom_point(size=2,fill="white", data=df.zebra, aes(x=Days,y=Distance.from.herd.opp.sp, shape=paste0(Species,1)))+
#   
#   geom_ribbon(aes(ymin=Min.distance.from.opp.herd, ymax=Max.distance.from.opp.herd, fill=Species),alpha=0.2)+
#   stat_smooth(formula=y~x, method="lm")+
#   stat_poly_eq(formula=y~x,
#                aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
#                color="black",parse=TRUE,label.x="left",label.y="top")+
#   
#   scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
#   scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
#   scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
#   
#   theme_classic()+
#   labs(y="Inter-herd distance (m)",x="Experiment day",fill="Species",color="Species",shape="Species")+
#   
#   theme_classic()+theme(
#     legend.position = "top"
#   )+
#   facet_wrap(~Species)


#-------------------------Total animals by day-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
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
  labs(y="Total animals",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


#-------------------------Total herds by day-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
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
  labs(y="Total herds",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)



#---------------------------------------------------------------------------------------------#
#------------------------------------How is vegetation affected-------------------------------#
#---------------------------------------------------------------------------------------------#

#-------------------------Veg % green by day (mean, sd)-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
ggplot(df %>% filter(Species %in% zebra.abbr), aes(x=Days,y=X.Green, color=Species, shape=Species, fill=Species))+
  geom_point(size=2)+
  
  geom_point(size=3, fill="white", data=ss %>% filter(Species %in% zebra.abbr), aes(x=Days,y=Mean.X.green, shape=paste0(Species,1)))+
  geom_errorbar(data=ss %>% filter(Species %in% zebra.abbr), aes(y=Mean.X.green, ymin=Mean.X.green-Sd.X.green, ymax=Mean.X.green+Sd.X.green))+
  stat_smooth(method="lm",formula=y~x)+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="right",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(1,16,2,17))+
  
  theme_classic()+
  labs(y="% Green",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


#-------------------------Veg % green by day (min, med, max)-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
ggplot(df.zebra, aes(x=Days, y=X.Green, color=Species, shape=Species, fill=Species))+
  geom_point(size=2, data=ss.zebra, aes(y=(Med.X.green)))+geom_line(data=ss.zebra, aes(y=Med.X.green),linetype="dashed")+
  geom_point(size=2, fill="white", aes(shape=paste0(Species,1)))+
  
  geom_ribbon(data=ss.zebra, aes(y=0, ymin=Min.X.green, ymax=Max.X.green, fill=Species),alpha=0.2)+
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="right",label.y="bottom")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="% Green",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


#-------------------------Veg % cover by day-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
ggplot(df.zebra, aes(x=Days,y=X.Cover, color=Species, shape=Species, fill=Species))+
  geom_point(size=2)+
  
  geom_point(size=3, fill="white", data=ss.zebra, aes(x=Days,y=Mean.X.cover, shape=paste0(Species,1)))+
  geom_errorbar(data=ss.zebra, aes(y=Mean.X.cover, ymin=Mean.X.cover-Sd.X.cover, ymax=Mean.X.cover+Sd.X.cover))+
  stat_smooth(method="lm",formula=y~x)+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(1,16,2,17))+
  
  theme_classic()+
  labs(y="% Cover",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


#-------------------------Veg % cover by day (min, med, max)-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
ggplot(df.zebra, aes(x=Days, y=X.Cover, color=Species, shape=Species, fill=Species))+
  geom_point(size=2, data=ss.zebra, aes(y=(Med.X.cover)))+geom_line(data=ss.zebra, aes(y=Med.X.cover),linetype="dashed")+
  geom_point(size=2, fill="white", aes(shape=paste0(Species,1)))+
  
  geom_ribbon(data=ss.zebra, aes(y=0, ymin=Min.X.cover, ymax=Max.X.cover, fill=Species),alpha=0.2)+
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="bottom")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="% Cover",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)



#---------------------------------------------------------------------------------------------#
#---------------------------How are density and vegetation linked?----------------------------#
#---------------------------------------------------------------------------------------------#

#-------------------------Density by % cover-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
ggplot(df %>% filter(Species %in% zebra.abbr), aes(x=X.Cover,y=Approximate.density, color=Species, shape=Species, fill=Species))+
  geom_point(size=2)+
  
  geom_point(size=3, fill="white", data=ss %>% filter(Species %in% zebra.abbr), aes(x=Mean.X.cover,y=Mean.density, shape=paste0(Species,1)))+
  geom_errorbar(data=ss %>% filter(Species %in% zebra.abbr), aes(x=Mean.X.cover,y=Mean.density, ymin=Mean.density-Sd.density, ymax=Mean.density+Sd.density))+
  geom_errorbarh(data=ss %>% filter(Species %in% zebra.abbr), aes(x=0, y=Mean.density,xmin=Mean.X.cover-Sd.X.cover, xmax=Mean.X.cover+Sd.X.cover))+
  stat_smooth(method="lm",formula=y~x)+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(1,16,2,17))+
  scale_x_continuous(labels = scales::percent)+
  
  theme_classic()+
  labs(y="Herd density",x="% Cover",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


#---------------------------------------------------------------------------------------------#
#--------------------------------What role do the cattle play?--------------------------------#
#---------------------------------------------------------------------------------------------#


#-------------------------Total cattle by day-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
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



#-------------------------Mob distance by day (min, med, max)-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
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
  labs(y="Distance from mob (m)",x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)



#-------------------------Cattle density by day-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
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
  labs(y=bquote("Cattle density (number of individuals within 2"~km~"standardized by area)"),x="Experiment day",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


#-------------------------Mob distance by herd size-------------------------#
ggplot(df.zebra, aes(x=Total.animals, y=Distance.from.mob, color=Species, shape=Species, fill=Species))+
  geom_point(size=3)+
  
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,17,1,2))+
  
  theme_classic()+
  labs(y="Distance from mob (m)",x="Herd size",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)



#-------------------------Cattle density by X.Green-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
ggplot(df.zebra, aes(x=Cattle.density, y=X.Green, color=Species, shape=Species, fill=Species))+
  geom_point(size=2)+
  
  # stat_smooth(formula=y~x, method="lm")+
  # stat_poly_eq(formula=y~x,
  #              aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
  #              color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,17))+
  scale_y_continuous(labels=scales::percent)+
  
  theme_classic()+
  labs(y="% Green",x="Cattle density",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)

#-------------------------Cattle density by X.Cover-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
ggplot(df.zebra, aes(x=Cattle.density, y=X.Cover, color=Species, shape=Species, fill=Species))+
  geom_point(size=2)+
  
  # stat_smooth(formula=y~x, method="lm")+
  # stat_poly_eq(formula=y~x,
  #              aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
  #              color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,17))+
  scale_y_continuous(labels=scales::percent)+
  
  theme_classic()+
  labs(y="% Cover",x="Cattle density",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)



#-------------------------Cattle density by herd size-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
ggplot(df.zebra, aes(x=Cattle.density, y=Total.animals, color=Species, shape=Species, fill=Species))+
  geom_point(size=2)+
  
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(1,2,16,17))+
  
  theme_classic()+
  labs(y="Herd size",x="Cattle density",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


#-------------------------Cattle density by herd size envelope-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
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


ggplot(zz, aes(x=Cattle.density, y=Max.herd.size, color=Species, shape=Species, fill=Species))+
  
  geom_point(size=2, aes(y=(Med.herd.size)))+geom_line(aes(y=Med.herd.size),linetype="dashed")+
  geom_point(size=2, fill="white", data=df.zebra, aes(y=Total.animals, shape=paste0(Species,1)))+
  
  geom_ribbon(aes(ymin=Min.herd.size, ymax=Max.herd.size, fill=Species),alpha=0.2)+
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
    aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
    color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,1,17,2))+
  
  theme_classic()+
  labs(y="Herd size",x=bquote("Cattle density (number of individuals within 2"~km~"standardized by area)"),fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


#-------------------------Mob distance by herd distance-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
ggplot(df.zebra, aes(x=Distance.from.herd, y=Distance.from.mob, color=Species, shape=Species, fill=Species))+
  geom_point(size=3)+
  
  stat_smooth(formula=y~x, method="lm")+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,17,1,2))+
  
  theme_classic()+
  labs(y="Distance from mob (m)",x="Inter-herd distance (m)",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


#-------------------------Mob distance by % green-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
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
  labs(x="Distance from mob (m)",y="% Green",fill="Species",color="Species",shape="Species")+

  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)



#-------------------------Mob distance by % cover-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
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
  labs(x="Distance from mob (m)",y="% Cover",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)





#---------------------------------------------------------------------------------------------#
#---------------------------------------Water access------------------------------------------#
#---------------------------------------------------------------------------------------------#

#-------------------------Distance to water by day (min, med, + max)-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
ggplot(ss.zebra, aes(x=Days, y=Max.distance.to.water, color=Species, shape=Species, fill=Species))+
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

ggplot(df.zebra, aes(x=Days, y=Distance.to.water, color=Species, shape=Species, fill=Species))+
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
#-------------------------------------Activity Ratio------------------------------------------#
#---------------------------------------------------------------------------------------------#

#-------------------------Grazing by days-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
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


#-------------------------Grazing by X.Cover and X.Green-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
library(gridExtra)
df.temp <- df.zebra %>% filter(Activity!="Grazing")
df.temp$Activity <- "Non-grazing"
df.zebra.grazing <- rbind(df.zebra %>% filter(Activity=="Grazing"),df.temp)
df.zebra.grazing$Group <- paste0(df.zebra.grazing$Activity,"_",df.zebra.grazing$Species)
p.cover <- round(summary(aov(X.Cover ~ Group, df.zebra.grazing))[[1]][1,5],4)
p.green <- round(summary(aov(X.Green ~ Group, df.zebra.grazing))[[1]][1,5],4)

gg_cover <- ggplot(df.zebra.grazing, aes(y=X.Cover,x=Group, fill=Group))+
  # annotate(geom = "rect", xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf)+
  geom_violin(trim=FALSE,alpha=0.6)+
  geom_boxplot(width=0.1,alpha=0.1)+
  stat_summary(fun=mean, geom="point", shape=23, size=3, fill="black")+
  
  scale_fill_manual(breaks=c("Non-grazing_GZ","Grazing_GZ","Non-grazing_PZ","Grazing_PZ"),labels=c("Grevy's zebra: Non-grazing","Grevy's zebra: General","Plains zebra: Non-grazing","Plains zebra: Grazing"),values=c(gz.2,lb,pz.2,lb))+
  scale_x_discrete(limits=c("Non-grazing_GZ","Grazing_GZ","Non-grazing_PZ","Grazing_PZ"),labels=c("Grevy's zebra: Non-grazing","Grevy's zebra: Grazing","Plains zebra: Non-grazing","Plains zebra: Grazing"))+
  scale_y_continuous(labels=scales::percent)+
  
  geom_text(aes(label=paste0("One-way ANOVA p=",p.cover),y=1.2, x=1.1))+
  
  theme_classic()+
  labs(x="",y="% Cover",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "none",
    axis.text.x=element_blank()
  )
gg_green <- ggplot(df.zebra.grazing, aes(y=X.Green,x=Group, fill=Group))+
  # annotate(geom = "rect", xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf)+
  geom_violin(trim=FALSE,alpha=0.6)+
  geom_boxplot(width=0.1,alpha=0.1)+
  stat_summary(fun=mean, geom="point", shape=23, size=3, fill="black")+
  
  scale_fill_manual(breaks=c("Non-grazing_GZ","Grazing_GZ","Non-grazing_PZ","Grazing_PZ"),labels=c("Grevy's zebra: General","Grevy's zebra: General","Plains zebra: General","Plains zebra: Grazing"),values=c(gz.2,lb,pz.2,lb))+
  scale_x_discrete(limits=c("Non-grazing_GZ","Grazing_GZ","Non-grazing_PZ","Grazing_PZ"),labels=c("Grevy's zebra: General","Grevy's zebra: Grazing","Plains zebra: General","Plains zebra: Grazing"))+
  scale_y_continuous(labels=scales::percent)+
  
  geom_text(aes(label=paste0("One-way ANOVA p=",p.green),y=0.8, x=1.1))+
  
  theme_classic()+
  labs(x="Grazing vs non-grazing composition by species",y="% Green",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "none"
  )
grid.arrange(gg_cover,gg_green,nrow=2)


#-------------------------Grazing by Cattle density-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
library(gridExtra)
df.temp <- df.zebra %>% filter(Activity!="Grazing")
df.temp$Activity <- "Non-grazing"
df.zebra.grazing <- rbind(df.zebra %>% filter(Activity=="Grazing"),df.temp)
df.zebra.grazing$Group <- paste0(df.zebra.grazing$Activity,"_",df.zebra.grazing$Species)
p.cattle <- round(summary(aov(Cattle.density ~ Group, df.zebra.grazing))[[1]][1,5],4)
TukeyHSD(aov(Cattle.density ~ Group, df.zebra.grazing))

ggplot(df.zebra.grazing, aes(y=Cattle.density,x=Group, fill=Group))+
  geom_violin(trim=FALSE,alpha=0.6)+
  geom_boxplot(width=0.1,alpha=0.1)+
  stat_summary(fun=mean, geom="point", shape=23, size=3, fill="black")+
  
  scale_fill_manual(breaks=c("Non-grazing_GZ","Grazing_GZ","Non-grazing_PZ","Grazing_PZ"),labels=c("Grevy's zebra: Non-grazing","Grevy's zebra: General","Plains zebra: Non-grazing","Plains zebra: Grazing"),values=c(gz.2,lb,pz.2,lb))+
  scale_x_discrete(limits=c("Non-grazing_GZ","Grazing_GZ","Non-grazing_PZ","Grazing_PZ"),labels=c("Grevy's zebra: Non-grazing","Grevy's zebra: Grazing","Plains zebra: Non-grazing","Plains zebra: Grazing"))+
  
  geom_text(aes(label=paste0("One-way ANOVA p=",p.cattle),x=3.85, y=120))+
  
  theme_classic()+
  labs(x="Grazing vs non-grazing composition by species",y=bquote("Cattle density (number of individuals within 2"~km~"standardized by area)"),fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "none"
  )




#---------------------------------------------------------------------------------------------#
#-------------------------------------Habitat Usage-------------------------------------------#
#---------------------------------------------------------------------------------------------#

#-------------------------OG by Cattle Density-------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
ggplot(ss.zebra, aes(x=Mean.cattle.density,y=X.OG, color=Species, shape=Species, fill=Species))+
  geom_point(size=3)+
  
  stat_smooth(method="lm",formula=y~x)+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,17))+
  scale_y_continuous(labels=scales::percent)+
  
  theme_classic()+
  labs(y="% Groups in open grassland",x="Cattle density",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


#-------------------------LB by Cattle Density-------------------------#
ggplot(ss.zebra, aes(x=Mean.cattle.density,y=X.LB, color=Species, shape=Species, fill=Species))+
  geom_point(size=3)+
  
  stat_smooth(method="lm",formula=y~x)+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,17))+
  scale_y_continuous(labels=scales::percent)+
  
  theme_classic()+
  labs(y="% Groups in light bush",x="Cattle density",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


#-------------------------MB by Cattle Density-------------------------#
ggplot(ss.zebra, aes(x=Mean.cattle.density,y=X.MB, color=Species, shape=Species, fill=Species))+
  geom_point(size=3)+
  
  stat_smooth(method="lm",formula=y~x)+
  stat_poly_eq(formula=y~x,
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left",label.y="top")+
  
  scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
  scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,17))+
  scale_y_continuous(labels=scales::percent)+
  
  theme_classic()+
  labs(y="% Groups in light bush",x="Cattle density",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  facet_wrap(~Species)


# ggplot(ss.zebra, aes(x=Total.cattle,y=X.MB, color=Species, shape=Species, fill=Species))+
#   geom_point(size=3)+
#   
#   stat_smooth(method="lm",formula=y~x)+
#   stat_poly_eq(formula=y~x,
#                aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
#                color="black",parse=TRUE,label.x="left",label.y="top")+
#   
#   scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
#   scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
#   scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(16,17))+
#   scale_y_continuous(labels=scales::percent)+
#   
#   theme_classic()+
#   labs(y="% Groups in light bush",x="Cattle density",fill="Species",color="Species",shape="Species")+
#   
#   theme_classic()+theme(
#     legend.position = "top"
#   )+
#   facet_wrap(~Species)




#---------------------------------------------------------------------------------------------#
#---------------------------------------AIC/BIC Analysis--------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
library(fitnesslandscapes2)
ss.zebra$Mean.NDVI <- ss.zebra$Mean.NDVI - ss.zebra$Random.NDVI
# df.zebra$NDVI <- 100*(df.zebra$NDVI - df.zebra$Random.NDVI)/df.zebra$Random.NDVI


# Best fit for total animals ~ top_down + bottom_up
summary(lm(Total.animals ~ NDVI + Distance.from.mob, df.zebra %>% filter(Species=="GZ")))
# nothing significant

TPS_landscape(df.zebra %>% filter(Species=="GZ", !is.na(NDVI), !is.na(Distance.from.mob)), x="NDVI", y="Distance.from.mob",z="Total.animals",output="contour", y_name="Distance from mob (m)")#x_name="NDVI (% better than average)",
TPS_distribution(df.zebra %>% filter(Species=="GZ", !is.na(X.Green), !is.na(Distance.from.mob)), x="NDVI", y="Distance.from.mob",output="contour",x_name="NDVI",y_name="Distance from mob (m)",pdf=T)


summary(lm(Total.animals ~ X.Green + Distance.from.mob, df.zebra %>% filter(Species=="PZ")))
# cattle p=0.01839, coeff=-0.002006825

TPS_landscape(df.zebra %>% filter(Species=="PZ", !is.na(X.Green), !is.na(Distance.from.mob)), x="X.Green", y="Distance.from.mob",z="Total.animals",output="contour",x_name="% Green",y_name="Distance from mob (m)")
TPS_distribution(df.zebra %>% filter(Species=="PZ", !is.na(X.Green), !is.na(Distance.from.mob)), x="X.Green", y="Distance.from.mob",output="contour",x_name="% Green",y_name="Distance from mob (m)",pdf=T)








#-------------------------Max herd size ss-------------------------#
AIC(lm(Max.herd.size ~ Total.cattle + Mean.NDVI + Total.cattle*Mean.NDVI, ss.zebra %>% filter(Species=="GZ")))
AIC(lm(Max.herd.size ~ Total.cattle + Total.cattle*Mean.NDVI, ss.zebra %>% filter(Species=="GZ")))
AIC(lm(Max.herd.size ~ Mean.NDVI + Total.cattle*Mean.NDVI, ss.zebra %>% filter(Species=="GZ")))
AIC(Max.GZ <- lm(Max.herd.size ~ Total.cattle + Mean.NDVI, ss.zebra %>% filter(Species=="GZ")))
AIC(lm(Max.herd.size ~ Mean.NDVI, ss.zebra %>% filter(Species=="GZ")))
AIC(lm(Max.herd.size ~ Total.cattle, ss.zebra %>% filter(Species=="GZ")))
summary(Max.GZ)
  # slope cattle: p=0.0932, NDVI: p=0.0101


AIC(lm(Max.herd.size ~ Total.cattle + Mean.NDVI + Total.cattle*Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
AIC(lm(Max.herd.size ~ Total.cattle + Total.cattle*Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
AIC(lm(Max.herd.size ~ Mean.NDVI + Total.cattle*Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
AIC(lm(Max.herd.size ~ Total.cattle + Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
AIC(Max.PZ <- lm(Max.herd.size ~ Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
AIC(lm(Max.herd.size ~ Total.cattle, ss.zebra %>% filter(Species=="PZ")))
summary(Max.PZ <- lm(Max.herd.size ~ Total.cattle + Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
  # slope cattle: p=0.879124, NDVI: p=0.303487


#-------------------------Herd size df-------------------------#
AIC(lm(Total.animals ~ Cattle.density + NDVI + Cattle.density*NDVI, df.zebra %>% filter(Species=="GZ")))
AIC(lm(Total.animals ~ Cattle.density + Cattle.density*NDVI, df.zebra %>% filter(Species=="GZ")))
AIC(lm(Total.animals ~ NDVI + Cattle.density*NDVI, df.zebra %>% filter(Species=="GZ")))
AIC(lm(Total.animals ~ Cattle.density + NDVI, df.zebra %>% filter(Species=="GZ")))
AIC(Tot.an.GZ.1 <- lm(Total.animals ~ Cattle.density, df.zebra %>% filter(Species=="GZ")))
AIC(Tot.an.GZ.2 <- lm(Total.animals ~ NDVI, df.zebra %>% filter(Species=="GZ")))
summary(Tot.an.GZ.1)
  # slope Cattle.density: p=0.635
summary(Tot.an.GZ.2)
  # slope NDVI: p=0.86


AIC(lm(Total.animals ~ Cattle.density + NDVI + X.Green + X.Cover + Distance.from.mob + Distance.from.herd, df.zebra %>% filter(Species=="GZ")))
AIC(lm(Total.animals ~ Cattle.density + X.Cover + Distance.from.mob + Distance.from.herd, df.zebra %>% filter(Species=="GZ")))
AIC(lm(Total.animals ~ X.Cover + Distance.from.mob + Distance.from.herd, df.zebra %>% filter(Species=="GZ")))










AIC(lm(Med.herd.size ~ Total.cattle + Mean.NDVI + Total.cattle*Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
AIC(lm(Med.herd.size ~ Total.cattle + Total.cattle*Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
AIC(lm(Med.herd.size ~ Mean.NDVI + Total.cattle*Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
AIC(lm(Med.herd.size ~ Total.cattle + Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
AIC(lm(Med.herd.size ~ Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
AIC(Med.PZ <- lm(Med.herd.size ~ Total.cattle, ss.zebra %>% filter(Species=="PZ")))
summary(Med.PZ)
  # slope cattle: p=0.0437



AIC(lm(Mean.distance.from.herd ~ Total.cattle + Mean.NDVI + Total.cattle*Mean.NDVI, ss.zebra %>% filter(Species=="GZ")))
AIC(lm(Mean.distance.from.herd ~ Total.cattle + Total.cattle*Mean.NDVI, ss.zebra %>% filter(Species=="GZ")))
AIC(lm(Mean.distance.from.herd ~ Mean.NDVI + Total.cattle*Mean.NDVI, ss.zebra %>% filter(Species=="GZ")))
AIC(lm(Mean.distance.from.herd ~ Total.cattle + Mean.NDVI, ss.zebra %>% filter(Species=="GZ")))
AIC(Med.GZ <- lm(Mean.distance.from.herd ~ Mean.NDVI, ss.zebra %>% filter(Species=="GZ")))
AIC(lm(Mean.distance.from.herd ~ Total.cattle, ss.zebra %>% filter(Species=="GZ")))
summary(Med.GZ)
# slope NDVI: p=0.341

AIC(lm(Mean.distance.from.herd ~ Total.cattle + Mean.NDVI + Total.cattle*Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
AIC(lm(Mean.distance.from.herd ~ Total.cattle + Total.cattle*Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
AIC(lm(Mean.distance.from.herd ~ Mean.NDVI + Total.cattle*Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
AIC(lm(Mean.distance.from.herd ~ Total.cattle + Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
AIC(lm(Mean.distance.from.herd ~ Mean.NDVI, ss.zebra %>% filter(Species=="PZ")))
AIC(Med.PZ <- lm(Mean.distance.from.herd ~ Total.cattle, ss.zebra %>% filter(Species=="PZ")))
summary(Med.PZ)
# slope cattle: p=0.0437




#---------------------------------------------------------------------------------------------#
#---------------------------------------PPR/TPS Analysis--------------------------------------#
#---------------------------------------------------------------------------------------------#

#-------------------------GZ-------------------------#
df.pp <- filter(df.zebra, Species=="GZ", !is.na(X.Green), !is.na(X.Cover), !is.na(NDVI), !is.na(EVI), !is.na(Distance.from.mob), !is.na(Distance.from.herd), !is.na(Cattle.density), !is.na(Approximate.density), !is.na(Total.animals))
df.norm <- as.data.frame(scale(df.pp[,c("X.Green","X.Cover","NDVI","EVI","Distance.from.mob","Distance.from.herd","Cattle.density")]))
explanatory <- as.matrix(df.norm)
response <- as.matrix(df.pp$Total.animals)
df.ppr <- stats::ppr(explanatory,response,nterms=2,maxterms=5)
pprdirections <- df.ppr$alpha
PP1 <- dotprod2(df.norm, pprdirections[,"term 1"])
PP2 <- dotprod2(df.norm, pprdirections[,"term 2"])
PPR_columns <- cbind(PP1,PP2)
colnames(PPR_columns) <- c("PP1","PP2")
output <- list(
  PPR_columns,
  pprdirections,
  df.ppr
)
names(output) <- c("columns","weights","ppr")
df.zebra[df.zebra$Species=="GZ" & !is.na(df.zebra$X.Green)& !is.na(df.zebra$X.Cover)& !is.na(df.zebra$NDVI)& !is.na(df.zebra$EVI)& !is.na(df.zebra$Distance.from.mob)& !is.na(df.zebra$Distance.from.herd)& !is.na(df.zebra$Cattle.density) & !is.na(df.zebra$Approximate.density)& !is.na(df.zebra$Total.animals),c("PP1","PP2")] <- output$columns
  # EVI, X.Green
TPS_landscape(df.zebra %>% filter(Species=="GZ", !is.na(PP1), !is.na(PP2)), z="Total.animals")


#------------------------PZ-------------------------#
df.pp <- filter(df.zebra, Species=="PZ", !is.na(X.Green), !is.na(X.Cover), !is.na(NDVI), !is.na(EVI), !is.na(Distance.from.mob), !is.na(Distance.from.herd), !is.na(Cattle.density), !is.na(Approximate.density), !is.na(Total.animals))
df.norm <- as.data.frame(scale(df.pp[,c("X.Green","X.Cover","NDVI","EVI","Distance.from.mob","Distance.from.herd","Cattle.density")]))
explanatory <- as.matrix(df.norm)
response <- as.matrix(df.pp$Total.animals)
df.ppr <- stats::ppr(explanatory,response,nterms=2,maxterms=5)
pprdirections <- df.ppr$alpha
PP1 <- dotprod2(df.norm, pprdirections[,"term 1"])
PP2 <- dotprod2(df.norm, pprdirections[,"term 2"])
PPR_columns <- cbind(PP1,PP2)
colnames(PPR_columns) <- c("PP1","PP2")
output <- list(
  PPR_columns,
  pprdirections,
  df.ppr
)
names(output) <- c("columns","weights","ppr")
df.zebra[df.zebra$Species=="PZ" & !is.na(df.zebra$X.Green)& !is.na(df.zebra$X.Cover)& !is.na(df.zebra$NDVI)& !is.na(df.zebra$EVI)& !is.na(df.zebra$Distance.from.mob)& !is.na(df.zebra$Distance.from.herd)& !is.na(df.zebra$Cattle.density) & !is.na(df.zebra$Approximate.density)& !is.na(df.zebra$Total.animals),c("PP1","PP2")] <- output$columns
# EVI, X.Green
TPS_landscape(df.zebra %>% filter(Species=="PZ", !is.na(PP1), !is.na(PP2)), z="Total.animals")







#---------------------------------------------------------------------------------------------#
#-------------------------------------AIC/BIC Analysis 2--------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')

ss$Mean.NDVI <- ss$Mean.NDVI-ss$Random.NDVI

ss.gz <- ss %>% filter(Exp.2!="Not in experiment", Species=="GZ")
summary(lm(Max.herd.size ~ Mean.cattle.density + Random.NDVI + Random.NDVI*Mean.cattle.density, ss.gz))

ss.pz <- ss %>% filter(Exp.2!="Not in experiment", Species=="PZ")
summary(lm(Max.herd.size ~ Total.cattle + Random.NDVI + Random.NDVI*Total.cattle, ss.pz))


summary(aov(Max.herd.size ~ Exp.1, ss.gz))
summary(aov(Max.herd.size ~ Exp.1, ss.pz))

summary(aov(Max.herd.size ~ Exp.2, ss.gz))
summary(aov(Max.herd.size ~ Exp.2, ss.pz))



#---------------------------------------------------------------------------------------------#
#-------------------------------------AIC/BIC Analysis 3--------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')

ss$Mean.NDVI.adj <- ss$Mean.NDVI-ss$Random.NDVI
df$NDVI.adj <- df$NDVI-df$Random.NDVI

ss.gz <- ss %>% filter(Species=="GZ")
ss.pz <- ss %>% filter(Species=="PZ")
df.gz <- df %>% filter(Species=="GZ")
df.pz <- df %>% filter(Species=="PZ")

AIC(lm(Total.animals ~ X.Green + NDVI + EVI + X.Cover + Cattle.density + Distance.from.mob, df.pz))

AIC(lm(Upper.herd.size ~ Upper.X.green + Upper.distance.from.mob + Upper.X.green*Upper.distance.from.mob, ss.pz))



AIC(lm(Distance.from.herd ~ Cattle.density, df.pz))
AIC(lm(Distance.from.herd ~ Cattle.density + Distance.from.mob, df.pz))
AIC(lm(Distance.from.herd ~ Distance.from.mob, df.pz))
AIC(lm(Distance.from.herd ~ Cattle.density + Distance.from.mob + NDVI, df.pz))
AIC(lm(Distance.from.herd ~ Cattle.density + Distance.from.mob + EVI, df.pz))
AIC(lm(Distance.from.herd ~ Cattle.density + Distance.from.mob + X.Cover, df.pz))
AIC(lm(Distance.from.herd ~ Cattle.density + Distance.from.mob + X.Green, df.pz))
AIC(lm(Distance.from.herd ~ Cattle.density + Distance.from.mob + X.Green*Distance.from.mob, df.pz))
AIC(lm(Distance.from.herd ~ Cattle.density + Distance.from.mob + X.Cover*Distance.from.mob, df.pz))
AIC(lm(Distance.from.herd ~ Cattle.density + Distance.from.mob, df.pz))
summary(lm(Distance.from.herd ~ Cattle.density + Distance.from.mob, df.pz))
#

AIC(lm(Mean.distance.from.herd ~ Mean.cattle.density, ss.pz))
AIC(lm(Mean.distance.from.herd ~ Mean.cattle.density + Mean.distance.to.water, ss.pz))
#


AIC(pzl <- lm(Mean.distance.from.herd ~ X.OG + X.LB + X.MB + Mean.cattle.density + Mean.distance.from.mob, ss.pz))
AIC(pzl <- lm(Mean.distance.from.herd ~ X.OG + X.LB + X.MB + Mean.distance.from.mob, ss.pz))
AIC(pzl <- lm(Mean.distance.from.herd ~ X.OG + X.LB + X.MB + Mean.distance.from.mob + Mean.X.cover, ss.pz))
AIC(pzl <- lm(Mean.distance.from.herd ~ X.OG + X.LB + X.MB + Mean.distance.from.mob + Mean.X.cover + Mean.X.cover*Mean.distance.from.mob, ss.pz))
AIC(pzl <- lm(Mean.distance.from.herd ~ X.OG + X.LB + X.MB + Mean.distance.from.mob + Mean.X.cover + Mean.X.cover*Mean.distance.from.mob + Mean.NDVI, ss.pz)) # EVI, x.green no good
AIC(pzl <- lm(Mean.distance.from.herd ~ X.OG + X.LB + X.MB + Mean.distance.from.mob + Mean.X.cover + Mean.X.cover*Mean.distance.from.mob + Mean.NDVI + Mean.NDVI*Mean.X.cover, ss.pz))
AIC(pzl <- lm(Mean.distance.from.herd ~ X.OG + X.LB + Mean.distance.from.mob + Mean.X.cover + Mean.X.cover*Mean.distance.from.mob + Mean.NDVI + Mean.NDVI*Mean.X.cover, ss.pz))
AIC(pzl <- lm(Mean.distance.from.herd ~ X.OG + X.LB + Mean.distance.from.mob + Mean.X.cover + Mean.X.cover*Mean.distance.from.mob + Mean.NDVI + Mean.NDVI*Mean.X.cover, ss.pz))
AIC(pzl <- lm(Mean.distance.from.herd ~ X.OG + X.LB + Mean.X.cover + Mean.X.cover*Mean.distance.from.mob + Mean.NDVI + Mean.NDVI*Mean.X.cover, ss.pz))
AIC(pzl <- lm(Mean.distance.from.herd ~ X.OG + X.LB + Mean.X.cover + Mean.X.cover*Mean.distance.from.mob + Mean.NDVI*Mean.X.cover, ss.pz))
AIC(pzl <- lm(Mean.distance.from.herd ~ X.OG + X.LB + Mean.X.cover + Mean.X.cover*Mean.distance.from.mob + Mean.NDVI*Mean.X.cover, ss.pz))
AIC(pzl <- lm(Mean.distance.from.herd ~ X.OG + X.LB + Mean.X.cover + Mean.distance.from.mob + Mean.NDVI + Mean.X.cover*Mean.distance.from.mob + Mean.NDVI*Mean.X.cover, ss.pz))
summary(pzl)


AIC(pzl <- lm(Mean.herd.size ~ X.OG + X.LB + Mean.X.cover + Mean.X.cover*Mean.distance.from.mob, ss.pz))
summary(pzl) ## Good
#










#---------------------------------------------------------------------------------------------#


summary(aov(Max.herd.size ~ Exp.1, ss.gz))
summary(aov(Max.herd.size ~ Exp.1, ss.pz))

summary(aov(Max.cattle.density ~ Exp.1, ss.gz))
summary(aov(Max.cattle.density ~ Exp.1, ss.pz))
summary(aov(Mean.cattle.density ~ Exp.1, ss.gz))
summary(aov(Mean.cattle.density ~ Exp.1, ss.pz))
summary(aov(Med.cattle.density ~ Exp.1, ss.gz))
summary(aov(Med.cattle.density ~ Exp.1, ss.pz))

summary(aov(Max.cattle.density ~ Exp.1 + Max.X.green, ss.gz))
summary(aov(Max.cattle.density ~ Exp.1 + Max.X.green, ss.pz))

summary(aov(Max.distance.from.mob ~ Exp.1, ss.gz))
summary(aov(Max.distance.from.mob ~ Exp.1, ss.pz))

summary(aov(Distance.from.mob ~ Exp.1, df.gz))
summary(aov(Distance.from.mob ~ Exp.1, df.pz))



#---------------------------------------------------------------------------------------------#
#-------------------------------------AIC/BIC Analysis 4--------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
library(ppcor)

ss$Mean.NDVI.adj <- ss$Mean.NDVI-ss$Random.NDVI
df$NDVI.adj <- df$NDVI-df$Random.NDVI

ss.gz <- ss %>% filter(Species=="GZ")
ss.pz <- ss %>% filter(Species=="PZ")
df.gz <- df %>% filter(Species=="GZ")
df.pz <- df %>% filter(Species=="PZ")


pzl <- lm(Mean.herd.size ~ X.OG + X.LB + Mean.X.cover + Mean.X.cover*Mean.distance.from.mob, ss.pz)
summary(pzl)

ss.pz.pcor <- ss.pz  %>% filter(!is.na(Mean.distance.from.mob))
pcor.test(ss.pz.pcor$Mean.herd.size, ss.pz.pcor$Mean.X.cover, ss.pz.pcor$Mean.distance.from.mob, method = c("pearson", "kendall", "spearman"))
pcor.test(ss.pz.pcor$Mean.herd.size, ss.pz.pcor$Mean.distance.from.mob, ss.pz.pcor$Mean.X.cover, method = c("pearson", "kendall", "spearman"))



AIC(gzl <- lm(Mean.herd.size ~ Mean.cattle.density + Mean.NDVI, ss.gz))
AIC(gzl <- lm(Mean.herd.size ~ Mean.cattle.density + Mean.NDVI + Mean.distance.from.mob, ss.gz))
AIC(gzl <- lm(Mean.herd.size ~ Mean.cattle.density + Mean.NDVI + Mean.distance.from.mob + Mean.distance.from.herd, ss.gz))
AIC(gzl <- lm(Mean.herd.size ~ Mean.cattle.density + Mean.NDVI + Mean.distance.from.mob + Mean.distance.from.herd + X.hock, ss.gz))
AIC(gzl <- lm(Mean.herd.size ~ Mean.cattle.density + Mean.NDVI + Mean.distance.from.mob + Mean.distance.from.herd + X.hock + X.hock*Mean.NDVI, ss.gz))
summary(lm(Mean.herd.size ~ Mean.cattle.density + Mean.NDVI + Mean.distance.from.mob + Mean.distance.from.herd + X.hock + X.hock*Mean.NDVI, ss.gz))

AIC(lm(Max.herd.size ~ Mean.cattle.density + Mean.NDVI + Mean.distance.from.mob + Mean.distance.from.herd + X.hock + X.hock*Mean.NDVI, ss.gz))
AIC(lm(Max.herd.size ~ Mean.cattle.density + Mean.NDVI + Mean.distance.from.mob + X.hock + X.hock*Mean.NDVI, ss.gz))
AIC(lm(Max.herd.size ~ Mean.cattle.density + Mean.NDVI + Mean.distance.from.mob + X.hock + X.hock*Mean.NDVI, ss.gz))
summary(lm(Max.herd.size ~ Mean.cattle.density + Mean.NDVI + Mean.distance.from.mob + X.hock + X.hock*Mean.NDVI, ss.gz))



#---------------------------------------------------------------------------------------------#
#-------------------------------------------ARCHIVE-------------------------------------------#
#---------------------------------------------------------------------------------------------#


# ggplot(df.zebra, aes(x=Days,y=Total.animals, color=Species, shape=Species, fill=Species))+
#   geom_point(size=2)+
#   
#   geom_point(size=0, fill="white", data=ss.zebra, aes(x=Days,y=Med.herd.size, shape=paste0(Species,1)))+
#   geom_ribbon(data=ss.zebra, aes(y=Med.herd.size, ymin=Min.herd.size, ymax=Max.herd.size, fill=Species),alpha=0.2)+
#   stat_smooth(data=ss.zebra, aes(x=Days, y=Max.herd.size), formula=y~x, method="lm")+
#   stat_poly_eq(#data=ss.zebra, aes(x=Days, y=Max.herd.size),
#                formula=y~x,
#                aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
#                color="black",parse=TRUE,label.x="left")
# 
#   
#   scale_color_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
#   scale_fill_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(gz.2,pz.2,"white"))+
#   scale_shape_manual(breaks=c(zebra.abbr,"mean"),labels=c(zebra.names,"Means"),values=c(1,16,2,17))+
#   
#   theme_classic()+
#   labs(y="Herd size",x="Experiment day",fill="Species",color="Species",shape="Species")+
#   
#   theme_classic()+theme(
#     legend.position = "top"
#   )+
#   facet_wrap(~Species)
