

#---------------------------------------------------------------------------------------------#
#-------------------------------------Herd size, NDVI-----------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss.zebra <- filter(ss, Species%in%c(zebra.abbr))
ss.zebra$SpeciesName <- factor(find_replace(ss.zebra$Species,data.frame(x=zebra.abbr,y=zebra.names)), levels=zebra.names)


gg_NDVI <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Days, y=Mean.NDVI, color=Species, shape=Species))+
  annotate(geom="rect",xmin=begin_phase3$before, xmax=end_phase3$before, ymin=0, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$during, xmax=end_phase3$during, ymin=0, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$after, xmax=end_phase3$after, ymin=0, ymax=Inf, fill=phase3.cols[3], alpha=.4)+
  # annotate(geom="rect",xmin=begin_phase3$end, xmax=end_phase3$end, ymin=0, ymax=Inf, fill="#bbbbbb", alpha=.4)+
  
  geom_point(size=3)+
  geom_errorbar(aes(ymin=Mean.NDVI-Sd.NDVI,ymax=Mean.NDVI+Sd.NDVI))+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="",y="NDVI",fill="Species",color="Species",shape="Species")+
  
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_blank(),
    legend.position = "top"
  )+
  
  facet_wrap(~SpeciesName)


gg_herdsize <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Days, y=Mean.herd.size, color=Species, shape=Species))+
  annotate(geom="rect",xmin=begin_phase3$before, xmax=end_phase3$before, ymin=0, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$during, xmax=end_phase3$during, ymin=0, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$after, xmax=end_phase3$after, ymin=0, ymax=Inf, fill=phase3.cols[3], alpha=.4)+
  # annotate(geom="rect",xmin=begin_phase3$end, xmax=end_phase3$end, ymin=0, ymax=Inf, fill="#bbbbbb", alpha=.4)+
  
  geom_point(size=3)+
  geom_errorbar(aes(ymin=Mean.herd.size-Sd.herd.size,ymax=Mean.herd.size+Sd.herd.size))+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="Date of observation",y="Herd size",fill="Species",color="Species",shape="Species")+
  
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "none"
  )+
  
  facet_wrap(~SpeciesName)

grid.arrange(gg_NDVI,gg_herdsize)



#---------------------------------------------------------------------------------------------#
#------------------------------------Herd density, EVI----------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss.zebra <- filter(ss, Species%in%c(zebra.abbr))
ss.zebra$SpeciesName <- factor(find_replace(ss.zebra$Species,data.frame(x=zebra.abbr,y=zebra.names)), levels=zebra.names)


gg_EVI <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Days, y=Mean.EVI, color=Species, shape=Species))+
  annotate(geom="rect",xmin=begin_phase3$before, xmax=end_phase3$before, ymin=0, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$during, xmax=end_phase3$during, ymin=0, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$after, xmax=end_phase3$after, ymin=0, ymax=Inf, fill=phase3.cols[3], alpha=.4)+
  # annotate(geom="rect",xmin=begin_phase3$end, xmax=end_phase3$end, ymin=0, ymax=Inf, fill="#bbbbbb", alpha=.4)+
  
  geom_point(size=3)+
  geom_errorbar(aes(ymin=Mean.EVI-Sd.EVI,ymax=Mean.EVI+Sd.EVI))+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="",y="EVI",fill="Species",color="Species",shape="Species")+
  
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_blank(),
    legend.position = "top"
  )+
  
  facet_wrap(~SpeciesName)


gg_density <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Days, y=Mean.density, color=Species, shape=Species))+
  annotate(geom="rect",xmin=begin_phase3$before, xmax=end_phase3$before, ymin=0, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$during, xmax=end_phase3$during, ymin=0, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$after, xmax=end_phase3$after, ymin=0, ymax=Inf, fill=phase3.cols[3], alpha=.4)+
  # annotate(geom="rect",xmin=begin_phase3$end, xmax=end_phase3$end, ymin=0, ymax=Inf, fill="#bbbbbb", alpha=.4)+
  
  geom_point(size=3)+
  geom_errorbar(aes(ymin=Mean.density-Sd.density,ymax=Mean.density+Sd.density))+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="Date of observation",y="Herd density",fill="Species",color="Species",shape="Species")+
  
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "none"
  )+
  
  facet_wrap(~SpeciesName)

grid.arrange(gg_EVI,gg_density)






#---------------------------------------------------------------------------------------------#
#----------------------------------VI by herd parameters--------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss.zebra <- filter(ss, Species%in%c(zebra.abbr))
ss.zebra$SpeciesName <- factor(find_replace(ss.zebra$Species,data.frame(x=zebra.abbr,y=zebra.names)), levels=zebra.names)


gg_NDVI_size <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Mean.NDVI, y=Mean.herd.size, color=Species, shape=Species))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="NDVI",y="Herd size",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    axis.text.x = element_blank(),
    legend.position = "top"
    )+
  
  facet_wrap(~SpeciesName)



gg_EVI_density <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Mean.EVI, y=Mean.density, color=Species, shape=Species))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="EVI",y="Herd density",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(legend.position = "none")+
  
  facet_wrap(~SpeciesName)

grid.arrange(gg_NDVI_size,gg_EVI_density)




#---------------------------------------------------------------------------------------------#
#---------------------------------Cattle by herd parameters-----------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss.zebra <- filter(ss, Species%in%c(zebra.abbr))
ss.zebra$SpeciesName <- factor(find_replace(ss.zebra$Species,data.frame(x=zebra.abbr,y=zebra.names)), levels=zebra.names)


gg_cattle_size <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Total.cattle, y=Mean.herd.size, color=Species, shape=Species))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="",y="Herd size",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+
  theme(
    axis.text.x = element_blank(),
    legend.position = "top"
  )+
  
  facet_wrap(~SpeciesName)


gg_cattle_density <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Total.cattle, y=Mean.density, color=Species, shape=Species))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="Total cattle",y="Herd density",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(legend.position = "none")+
  
  facet_wrap(~SpeciesName)

grid.arrange(gg_cattle_size,gg_cattle_density)




#---------------------------------------------------------------------------------------------#
#---------------------------------------1st Analysis------------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss.zebra <- filter(ss, Species%in%c(zebra.abbr))

summary(lm(Mean.NDVI ~ poly(Mean.NDVI,2), filter(ss.zebra,Species=="GZ", !is.na(Mean.NDVI))))
summary(lm(Mean.NDVI ~ poly(Mean.NDVI,2), filter(ss.zebra,Species=="PZ", !is.na(Mean.NDVI))))

summary(lm(Mean.herd.size ~ poly(Days,2), filter(ss.zebra,Species=="GZ")))
summary(lm(Mean.herd.size ~ poly(Days,2), filter(ss.zebra,Species=="PZ")))


summary(lm(Mean.EVI ~ poly(Mean.EVI,2), filter(ss.zebra,Species=="GZ", !is.na(Mean.NDVI))))
summary(lm(Mean.EVI ~ poly(Mean.EVI,2), filter(ss.zebra,Species=="PZ", !is.na(Mean.NDVI))))

summary(lm(Mean.density ~ poly(Days,2), filter(ss.zebra,Species=="GZ")))
summary(lm(Mean.density ~ poly(Days,2), filter(ss.zebra,Species=="PZ")))


summary(lm(Mean.herd.size ~ poly(Mean.NDVI,2), filter(ss.zebra,Species=="GZ", !is.na(Mean.NDVI))))
summary(lm(Mean.herd.size ~ poly(Mean.NDVI,2), filter(ss.zebra,Species=="PZ", !is.na(Mean.NDVI))))

summary(lm(Mean.density ~ poly(Mean.EVI,2), filter(ss.zebra,Species=="GZ", !is.na(Mean.NDVI))))
summary(lm(Mean.density ~ poly(Mean.EVI,2), filter(ss.zebra,Species=="PZ", !is.na(Mean.NDVI))))

summary(lm(Mean.herd.size ~ poly(Total.cattle,2), filter(ss.zebra,Species=="GZ")))
summary(lm(Mean.herd.size ~ poly(Total.cattle,2), filter(ss.zebra,Species=="PZ")))

summary(lm(Mean.density ~ poly(Total.cattle,2), filter(ss.zebra,Species=="GZ")))
summary(lm(Mean.density ~ poly(Total.cattle,2), filter(ss.zebra,Species=="PZ")))





#---------------------------------------------------------------------------------------------#
#------------------------------------VI by % grazing------------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss.zebra <- filter(ss, Species%in%c(zebra.abbr))
ss.zebra$SpeciesName <- factor(find_replace(ss.zebra$Species,data.frame(x=zebra.abbr,y=zebra.names)), levels=zebra.names)


gg_NDVI_grazing <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Mean.NDVI, y=X.Grazing, color=Species, shape=Species))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="NDVI",y="% Grazing",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  
  facet_wrap(~SpeciesName)



gg_EVI_grazing <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Mean.EVI, y=X.Grazing, color=Species, shape=Species))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="EVI",y="% Grazing",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(legend.position = "none")+
  
  facet_wrap(~SpeciesName)

grid.arrange(gg_NDVI_grazing,gg_EVI_grazing)


#---------------------------------------------------------------------------------------------#
#---------------------------------VI by total cattle------------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss.zebra <- filter(ss, Species%in%c(zebra.abbr))
ss.zebra$SpeciesName <- factor(find_replace(ss.zebra$Species,data.frame(x=zebra.abbr,y=zebra.names)), levels=zebra.names)


gg_NDVI_cattle <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Total.cattle, y=Mean.NDVI, color=Species, shape=Species))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="",y="NDVI",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    axis.text.x = element_blank(),
    legend.position = "top"
  )+
  
  facet_wrap(~SpeciesName)



gg_EVI_cattle <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Total.cattle, y=Mean.EVI, color=Species, shape=Species))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="Total cattle",y="EVI",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(legend.position = "none")+
  
  facet_wrap(~SpeciesName)

grid.arrange(gg_NDVI_cattle,gg_EVI_cattle)



#---------------------------------------------------------------------------------------------#
#---------------------------------VI by mob distance------------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss.zebra <- filter(ss, Species%in%c(zebra.abbr), Mean.distance.from.mob<=5e3)
ss.zebra$SpeciesName <- factor(find_replace(ss.zebra$Species,data.frame(x=zebra.abbr,y=zebra.names)), levels=zebra.names)

gg_NDVI_mob <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Mean.distance.from.mob, y=Mean.NDVI, color=Species, shape=Species))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="Mean distance from mob",y="NDVI",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  
  facet_wrap(~SpeciesName)



gg_EVI_mob <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Mean.distance.from.mob, y=Mean.EVI, color=Species, shape=Species))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="Mean distance from mob",y="EVI",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(legend.position = "none")+
  
  facet_wrap(~SpeciesName)

grid.arrange(gg_NDVI_mob,gg_EVI_mob)






#---------------------------------------------------------------------------------------------#
#---------------------------------------2nd Analysis------------------------------------------#
#---------------------------------------------------------------------------------------------#


summary(lm(X.Grazing ~ poly(Mean.NDVI,2), filter(ss.zebra,Species=="GZ")))
summary(lm(X.Grazing ~ poly(Mean.NDVI,2), filter(ss.zebra,Species=="PZ")))

summary(lm(X.Grazing ~ poly(Mean.EVI,2), filter(ss.zebra,Species=="GZ")))
summary(lm(X.Grazing ~ poly(Mean.EVI,2), filter(ss.zebra,Species=="PZ")))


summary(lm(Mean.NDVI ~ poly(Total.cattle,2), filter(ss.zebra,Species=="GZ", !is,na(Mean.NDVI))))
summary(lm(Mean.NDVI ~ poly(Total.cattle,2), filter(ss.zebra,Species=="PZ", !is,na(Mean.NDVI))))

summary(lm(Mean.EVI ~ poly(Total.cattle,2), filter(ss.zebra,Species=="PZ")))
summary(lm(Mean.EVI ~ poly(Total.cattle,2), filter(ss.zebra,Species=="PZ")))


summary(lm(Mean.NDVI ~ poly(Mean.distance.from.mob,2), filter(ss.zebra,Species=="GZ", !is,na(Mean.NDVI))))
summary(lm(Mean.NDVI ~ poly(Mean.distance.from.mob,2), filter(ss.zebra,Species=="PZ", !is,na(Mean.NDVI))))

summary(lm(Mean.EVI ~ poly(Mean.distance.from.mob,2), filter(ss.zebra,Species=="GZ", !is,na(Mean.EVI))))
summary(lm(Mean.EVI ~ poly(Mean.distance.from.mob,2), filter(ss.zebra,Species=="PZ", !is,na(Mean.EVI))))






#---------------------------------------------------------------------------------------------#
#------------------------------------X.green, X.cover---------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss.zebra <- filter(ss, Species%in%c(zebra.abbr))
ss.zebra$SpeciesName <- factor(find_replace(ss.zebra$Species,data.frame(x=zebra.abbr,y=zebra.names)), levels=zebra.names)


gg_cover <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Days, y=Mean.X.cover, color=Species, shape=Species))+
  annotate(geom="rect",xmin=begin_phase3$before, xmax=end_phase3$before, ymin=0, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$during, xmax=end_phase3$during, ymin=0, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$after, xmax=end_phase3$after, ymin=0, ymax=Inf, fill=phase3.cols[3], alpha=.4)+
  # annotate(geom="rect",xmin=begin_phase3$end, xmax=end_phase3$end, ymin=0, ymax=Inf, fill="#bbbbbb", alpha=.4)+
  
  geom_point(size=3)+
  geom_errorbar(aes(ymin=Mean.X.cover-Sd.X.cover,ymax=Mean.X.cover+Sd.X.cover))+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  stat_poly_eq(formula=y~poly(x,2), 
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.y="bottom")+
  
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="",y="% Cover",fill="Species",color="Species",shape="Species")+
  
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_blank(),
    legend.position = "top"
  )+
  
  facet_wrap(~SpeciesName)


gg_green <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Days, y=Mean.X.green, color=Species, shape=Species))+
  annotate(geom="rect",xmin=begin_phase3$before, xmax=end_phase3$before, ymin=0, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$during, xmax=end_phase3$during, ymin=0, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$after, xmax=end_phase3$after, ymin=0, ymax=Inf, fill=phase3.cols[3], alpha=.4)+
  # annotate(geom="rect",xmin=begin_phase3$end, xmax=end_phase3$end, ymin=0, ymax=Inf, fill="#bbbbbb", alpha=.4)+
  
  geom_point(size=3)+
  geom_errorbar(aes(ymin=Mean.X.green-Sd.X.green,ymax=Mean.X.green+Sd.X.green))+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  stat_poly_eq(formula=y~poly(x,2), 
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.y="bottom")+
  
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="Date of observation",y="% Green",fill="Species",color="Species",shape="Species")+
  
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "none"
  )+
  
  facet_wrap(~SpeciesName)

grid.arrange(gg_cover,gg_green)




#---------------------------------------------------------------------------------------------#
#-------------------------------X.green ~ size, X.cover ~ density-----------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss.zebra <- filter(ss, Species%in%c(zebra.abbr))
ss.zebra$SpeciesName <- factor(find_replace(ss.zebra$Species,data.frame(x=zebra.abbr,y=zebra.names)), levels=zebra.names)

gg_cover_size <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Mean.X.cover, y=Mean.herd.size, color=Species, shape=Species))+
  geom_point(size=3)+
  
  stat_smooth(method="lm",formula=y~poly(x,1))+
  stat_poly_eq(formula=y~poly(x,1), 
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left")+
  
  scale_x_continuous(labels = scales::percent)+
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="% Cover",y="Herd size",fill="Species",color="Species",shape="Species")+

  theme_classic()+theme(
    legend.position = "top"
  )+
  
  facet_wrap(~SpeciesName)


gg_green_density <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Mean.X.green, y=Mean.density, color=Species, shape=Species))+
  geom_point(size=3)+
  
  stat_smooth(method="lm",formula=y~poly(x,1))+
  stat_poly_eq(formula=y~poly(x,1), 
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left")+
  
  scale_x_continuous(labels = scales::percent)+
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="% Green",y="Herd density",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "none"
  )+
  
  facet_wrap(~SpeciesName)

grid.arrange(gg_cover_size,gg_green_density)

#---------------------------------------------------------------------------------------------#
#-------------------------------X.green ~ size, X.cover ~ density-----------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss.zebra <- filter(ss, Species%in%c(zebra.abbr))
ss.zebra$SpeciesName <- factor(find_replace(ss.zebra$Species,data.frame(x=zebra.abbr,y=zebra.names)), levels=zebra.names)

gg_cover_size <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(y=Mean.X.green, x=Mean.herd.size, color=Species, shape=Species))+
  geom_point(size=3)+
  
  stat_smooth(method="lm",formula=y~poly(x,1))+
  stat_poly_eq(formula=y~poly(x,1), 
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left")+
  
  scale_x_continuous(labels = scales::percent)+
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(y="% Green",x="Herd size",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "top"
  )+
  
  facet_wrap(~SpeciesName)


gg_green_density <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(y=Mean.X.cover, x=Mean.density, color=Species, shape=Species))+
  geom_point(size=3)+
  
  stat_smooth(method="lm",formula=y~poly(x,1))+
  stat_poly_eq(formula=y~poly(x,1), 
               aes(label=paste(..rr.label.., p.value.label, sep = "~~~")),
               color="black",parse=TRUE,label.x="left")+
  
  scale_x_continuous(labels = scales::percent)+
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(y="% Cover",x="Herd density",fill="Species",color="Species",shape="Species")+
  
  theme_classic()+theme(
    legend.position = "none"
  )+
  
  facet_wrap(~SpeciesName)

grid.arrange(gg_cover_size,gg_green_density)



#---------------------------------------------------------------------------------------------#
#-------------------------------X.green ~ size, X.cover ~ density analysis-----------------------------#
#---------------------------------------------------------------------------------------------#
library(vcd)

shapiro.test(ss$Mean.X.cover) # p=0.08929
shapiro.test(ss$Mean.X.green) # p=0.1318
summary(goodfit(filter(ss,Species=="GZ")$Mean.density,type= "poisson",method= "ML"))
summary(goodfit(filter(ss,Species=="GZ")$Mean.herd.size,type= "poisson",method= "ML"))
summary(goodfit(filter(ss,Species=="PZ")$Mean.density,type= "poisson",method= "ML"))
summary(goodfit(filter(ss,Species=="PZ")$Mean.herd.size,type= "poisson",method= "ML"))


summary(lm(Mean.X.cover ~ Mean.density + Total.cattle + 0, filter(ss, Species=="GZ")))

ss.gz <- filter(ss, Species=="GZ")
ss.gz.scale <- cbind(ss.gz$Mean.density,as.data.frame(scale(ss.gz %>% dplyr::select(Mean.X.cover,Total.cattle))))
colnames(ss.gz.scale) <- c("Mean.density", "Mean.X.cover", "Total.cattle")
summary(g.gz.scale <- glm(Mean.density ~ Mean.X.cover + Total.cattle, ss.gz.scale, family="poisson"))
g.gz.scale$coefficients
TPS_landscape(ss.gz,x="Mean.X.cover",y="Total.cattle",z="Mean.density")

ss.pz <- filter(ss, Species=="PZ")
ss.pz.scale <- cbind(ss.pz$Mean.density,as.data.frame(scale(ss.pz %>% dplyr::select(Mean.X.cover,Total.cattle))))
colnames(ss.pz.scale) <- c("Mean.density", "Mean.X.cover", "Total.cattle")
summary(g.pz.scale <- glm(Mean.density ~ Mean.X.cover + Total.cattle, ss.pz.scale, family="poisson"))
g.pz.scale$coefficients
TPS_landscape(ss.pz,x="Mean.X.cover",y="Total.cattle",z="Mean.density")



ss.gz <- filter(ss, Species=="GZ")
ss.gz.scale <- cbind(ss.gz$Mean.herd.size,as.data.frame(scale(ss.gz %>% dplyr::select(Mean.X.green,Total.cattle))))
colnames(ss.gz.scale) <- c("Mean.herd.size", "Mean.X.green", "Total.cattle")
summary(g.gz.scale <- glm(Mean.herd.size ~ Mean.X.green + Total.cattle, ss.gz.scale, family="poisson"))
g.gz.scale$coefficients
TPS_landscape(ss.gz,x="Mean.X.cover",y="Total.cattle",z="Mean.herd.size")

ss.pz <- filter(df, Species=="PZ")
ss.pz.scale <- cbind(ss.gz$Mean.herd.size,as.data.frame(scale(ss.pz %>% dplyr::select(Mean.X.green,Total.cattle))))
colnames(ss.pz.scale) <- c("Mean.herd.size", "Mean.X.green", "Total.cattle")
summary(g.pz.scale <- glm(Mean.herd.size ~ Mean.X.green + Total.cattle, ss.pz.scale, family="poisson"))
g.pz.scale$coefficients
TPS_landscape(ss.pz,x="Mean.X.cover",y="Total.cattle",z="Mean.herd.size")





library(fitnesslandscapes2)
TPS_landscape(ss.gz,x="Mean.X.green",y="Total.cattle",z="Mean.herd.size")







#---------------------------------------------------------------------------------------------#
#-------------------------------------Herd size, Water-----------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss.zebra <- filter(ss, Species%in%c(zebra.abbr))
ss.zebra$SpeciesName <- factor(find_replace(ss.zebra$Species,data.frame(x=zebra.abbr,y=zebra.names)), levels=zebra.names)


gg_NDVI <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Days, y=Mean.distance.to.water, color=Species, shape=Species))+
  annotate(geom="rect",xmin=begin_phase3$before, xmax=end_phase3$before, ymin=0, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$during, xmax=end_phase3$during, ymin=0, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$after, xmax=end_phase3$after, ymin=0, ymax=Inf, fill=phase3.cols[3], alpha=.4)+
  # annotate(geom="rect",xmin=begin_phase3$end, xmax=end_phase3$end, ymin=0, ymax=Inf, fill="#bbbbbb", alpha=.4)+
  
  geom_point(size=3)+
  geom_errorbar(aes(ymin=Mean.distance.to.water-Sd.distance.to.water,ymax=Mean.distance.to.water+Sd.distance.to.water))+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="",y="Distance to water",fill="Species",color="Species",shape="Species")+
  
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_blank(),
    legend.position = "top"
  )+
  
  facet_wrap(~SpeciesName)


gg_herdsize <- ggplot(filter(ss.zebra, Exp.1!="Not in experiment"), aes(x=Days, y=Mean.herd.size, color=Species, shape=Species))+
  annotate(geom="rect",xmin=begin_phase3$before, xmax=end_phase3$before, ymin=0, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$during, xmax=end_phase3$during, ymin=0, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$after, xmax=end_phase3$after, ymin=0, ymax=Inf, fill=phase3.cols[3], alpha=.4)+
  # annotate(geom="rect",xmin=begin_phase3$end, xmax=end_phase3$end, ymin=0, ymax=Inf, fill="#bbbbbb", alpha=.4)+
  
  geom_point(size=3)+
  geom_errorbar(aes(ymin=Mean.herd.size-Sd.herd.size,ymax=Mean.herd.size+Sd.herd.size))+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  
  scale_color_manual(breaks=zebra.abbr,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.abbr,labels=zebra.names,values=c(16,17))+
  theme_classic()+
  labs(x="Date of observation",y="Herd size",fill="Species",color="Species",shape="Species")+
  
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "none"
  )+
  
  facet_wrap(~SpeciesName)

grid.arrange(gg_NDVI,gg_herdsize)
