#---------------------------------------------------------------------------------------------#
#----------------------------------------% Activity-------------------------------------------#
#---------------------------------------------------------------------------------------------#
order_activity <- c("X.Grazing","X.Resting","X.Walking","X.Standing","X.Drinking")
activity_names <- gsub("X.","% ",order_activity)
mss <- melt(ss,measure.vars=order_activity)
mss$variable <- factor(mss$variable, levels=order_activity)


# zebra.abbr,"Cattle"
ggplot(filter(mss, Species%in%c("Cattle")), aes(x=Days, y=value, color=variable, fill=variable, shape=variable))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  facet_wrap(~Species)+
  scale_color_manual(labels=activity_names,values=activity.cols.3)+
  scale_fill_manual(labels=activity_names,values=activity.cols.3)+
  scale_shape_manual(labels=activity_names,values=c(16,17,15,23,25))+
  theme_classic()+
  labs(x="Date of observation",y="% Individuals doing activity",fill="Activity",color="Activity",shape="Activity")+
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "top"
  )+
  scale_y_continuous(labels = scales::percent, breaks=c(0,.25,.5,.75,1))


#---------------------------------------------------------------------------------------------#
#----------------------------------------% Activity 2-----------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss$SpeciesName <- factor(find_replace(ss$Species,data.frame(x=c(zebra.abbr,"Cattle"),y=c(zebra.names,"Cattle"))), levels=c(zebra.names,"Cattle"))
order_activity <- c("X.Grazing","X.Resting","X.Walking","X.Standing","X.Drinking")
activity_names <- gsub("X.","% ",order_activity)
mss <- melt(ss,measure.vars=order_activity)
mss$variable <- factor(mss$variable, levels=order_activity)
levels(mss$variable) <- activity_names

ggplot(filter(mss, Species%in%c(zebra.abbr,"Cattle"), variable%in%c("% Standing")), aes(x=Days, y=value, color=Species, fill=Species, shape=Species))+
  # annotate(geom="rect",xmin=begin_phase3$before, xmax=end_phase3$before, ymin=-Inf, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
  # annotate(geom="rect",xmin=begin_phase3$during, xmax=end_phase3$during, ymin=-Inf, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
  # annotate(geom="rect",xmin=begin_phase3$after, xmax=end_phase3$after, ymin=-Inf, ymax=Inf, fill=phase3.cols[3], alpha=.4)+
  # annotate(geom="rect",xmin=begin_phase3$end, xmax=end_phase3$end, ymin=-Inf, ymax=Inf, fill="#bbbbbb", alpha=.4)+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  scale_color_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(gz.2,pz.2,cattle))+
  scale_fill_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(gz.2,pz.2,cattle))+
  scale_shape_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(16,17,15))+
  theme_classic()+
  labs(x="Date of observation",y="% Groups standing",fill="Species",color="Species",shape="Species")+ # Individuals doing activity
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "top"
  )+
  scale_y_continuous(labels = scales::percent, breaks=c(0,.25,.5,.75,1))+
  facet_wrap(~Size+SpeciesName)


#---------------------------------------------------------------------------------------------#
#-----------------------------------% Activity Analysis---------------------------------------#
#---------------------------------------------------------------------------------------------#

Anova(aov(X.Resting ~ SpeciesName + Size + Exp.1 + SpeciesName*Size + SpeciesName*Exp.1 + Exp.1*Size + Exp.1*Size*SpeciesName, ss), type="II")
Anova(aov(X.Resting ~ SpeciesName + Size + Exp.1 + Total.cattle, ss), type="II")
kruskal.test(X.Resting ~ Exp.1, filter(ss, Species=="GZ",Exp.1!="Not in experiment"))



#---------------------------------------------------------------------------------------------#
#----------------------------------------Herd size--------------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ggplot(filter(ss, Species%in%c(zebra.abbr)), aes(x=Days, y=Mean.herd.size, color=Species, shape=Species))+
  annotate(geom="rect",xmin=begin_phase3$before, xmax=end_phase3$before, ymin=0, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$during, xmax=end_phase3$during, ymin=0, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$after, xmax=end_phase3$after, ymin=0, ymax=Inf, fill=phase3.cols[3], alpha=.4)+
  annotate(geom="rect",xmin=begin_phase3$end, xmax=end_phase3$end, ymin=0, ymax=Inf, fill="#bbbbbb", alpha=.4)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=Mean.herd.size-Sd.herd.size,ymax=Mean.herd.size+Sd.herd.size))+
  scale_color_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(gz.2,pz.2,cattle))+
  scale_shape_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(16,17,15))+
  # stat_smooth(method="lm",formula=y~poly(x,2))+
  stat_smooth(method="loess",formula=y~x)+
  theme_classic()+
  labs(x="Date of observation",y="Herd size",fill="Species",color="Species",shape="Species")+
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "top"
  )+
  facet_wrap(~Species)

#---------------------------------------------------------------------------------------------#
#------------------------------------Distance to herd-----------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ggplot(filter(ss, Species%in%c(zebra.abbr), Mean.distance.from.herd<=40000, Sd.distance.from.herd<=3000), aes(x=Days, y=Mean.distance.from.herd, color=Species, shape=Species))+
  annotate(geom="rect",xmin=begin_phase3$before, xmax=end_phase3$before, ymin=0, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$during, xmax=end_phase3$during, ymin=0, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$after, xmax=end_phase3$after, ymin=0, ymax=Inf, fill=phase3.cols[3], alpha=.4)+
  annotate(geom="rect",xmin=begin_phase3$end, xmax=end_phase3$end, ymin=0, ymax=Inf, fill="#bbbbbb", alpha=.4)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=Mean.distance.from.herd-Sd.distance.from.herd,ymax=Mean.distance.from.herd+Sd.distance.from.herd))+
  scale_color_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(gz.2,pz.2,cattle))+
  scale_shape_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(16,17,15))+
  # stat_smooth(method="lm",formula=y~poly(x,2))+
  theme_classic()+
  labs(x="Date of observation",y="Herd size",fill="Species",color="Species",shape="Species")+
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "top"
  )+
  facet_wrap(~Species)


#---------------------------------------------------------------------------------------------#
#-----------------------------------------Density---------------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss$SpeciesName <- factor(find_replace(ss$Species,data.frame(x=c(zebra.abbr,"Cattle"),y=c(zebra.names,"Cattle"))), levels=c(zebra.names,"Cattle"))
ggplot(filter(ss, Species%in%c(zebra.abbr)), aes(x=Days, y=Mean.density, color=Species, shape=Species))+
  annotate(geom="rect",xmin=begin_phase3$before, xmax=end_phase3$before, ymin=0, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$during, xmax=end_phase3$during, ymin=0, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$after, xmax=end_phase3$after, ymin=0, ymax=Inf, fill=phase3.cols[3], alpha=.4)+
  annotate(geom="rect",xmin=begin_phase3$end, xmax=end_phase3$end, ymin=0, ymax=Inf, fill="#bbbbbb", alpha=.4)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=Mean.density-Sd.density,ymax=Mean.density+Sd.density))+
  scale_color_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(gz.2,pz.2,cattle))+
  scale_shape_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(16,17,15))+
  # stat_smooth(method="lm",formula=y~x)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  # stat_smooth(method="loess",formula=y~x)+
  theme_classic()+
  labs(x="Date of observation",y="Herd density",fill="Species",color="Species",shape="Species")+
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "top"
  )+
  facet_wrap(~SpeciesName)



#---------------------------------------------------------------------------------------------#
#-----------------------------------Herd size & density---------------------------------------#
#---------------------------------------------------------------------------------------------#

Anova(aov(Mean.herd.size ~ Species + Exp.1 + Species*Exp.1, ss), type="II")

kruskal.test(Mean.herd.size ~ Exp.1, filter(ss, Species=="GZ"))
kruskal.test(Sd.herd.size ~ Exp.1, filter(ss, Species=="PZ"))

Anova(aov(Mean.density ~ Species + Exp.1 + Species*Exp.1, ss), type="II")
Anova(aov(Sd.density ~ Species + Exp.1 + Species*Exp.1, ss), type="II")

ss$Phase_Species_Size <- paste0(ss$Exp.1,"_",ss$Species,"_",ss$Size)
kruskal.test(Sd.density/Mean.density ~ Phase_Species, ss)
kruskal.test(Mean.density ~ Phase_Species, ss)
kruskal.test(Mean.density ~ Exp.1, ss)



#---------------------------------------------------------------------------------------------#
#----------------------------------Mean.distance.from.herd------------------------------------#
#---------------------------------------------------------------------------------------------#
ggplot(filter(ss, Species%in%c(zebra.abbr)), aes(x=Days, y=Mean.distance.from.herd, color=Species, shape=Species))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=Mean.distance.from.herd-Sd.distance.from.herd,ymax=Mean.distance.from.herd+Sd.distance.from.herd))+
  scale_color_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(gz.2,pz.2,cattle))+
  scale_shape_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(16,17,15))+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  theme_classic()+
  labs(x="Date of observation",y="Herd size",fill="Species",color="Species",shape="Species")+
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "top"
  )+facet_wrap(~Species)



#---------------------------------------------------------------------------------------------#
#----------------------------------Distance from mob------------------------------------------#
#---------------------------------------------------------------------------------------------#
ggplot(filter(ss, Species%in%c(zebra.abbr)), aes(x=Days, y=Mean.distance.from.mob, color=Species, shape=Species))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=Mean.distance.from.mob-Sd.distance.from.mob,ymax=Mean.distance.from.mob+Sd.distance.from.mob))+
  scale_color_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(gz.2,pz.2,cattle))+
  scale_shape_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(16,17,15))+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  theme_classic()+
  labs(x="Date of observation",y="Distance from mob (m)",fill="Species",color="Species",shape="Species")+
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "top"
  )+facet_wrap(~Species)


#
# annotate(geom="rect",xmin=mdy("6/3/21"), xmax=mdy("6/14/21"), ymin=0, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
#   annotate(geom="rect", xmin=mdy("6/14/21"), xmax=mdy("6/18/21"), ymin=0, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
#   annotate(geom="rect", xmin=mdy("6/18/21"), xmax=today(), ymin=0, ymax=Inf, fill=phase3.cols[3], alpha=.4)+


#---------------------------------------------------------------------------------------------#
#----------------------------------------% Bush Type------------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss$SpeciesName <- factor(find_replace(ss$Species,data.frame(x=c(zebra.abbr,"Cattle"),y=c(zebra.names,"Cattle"))), levels=c(zebra.names,"Cattle"))
order_bush <- c("X.open.grassland","X.light","X.medium","X.thick")
bush_names <- find_replace(order_bush,data.frame(x=order_bush,y=c("% Open grassland","% Light bush","% Medium bush","% Thick bush")))
mss <- melt(ss,measure.vars=order_bush)
mss$variable <- factor(mss$variable, levels=order_bush)
levels(mss$variable) <- bush_names

ggplot(filter(mss, Species%in%c(zebra.abbr,"Cattle"), variable%in%bush_names), aes(x=Days, y=value, color=Species, fill=Species, shape=Species))+
  annotate(geom="rect",xmin=begin_phase3$before, xmax=end_phase3$before, ymin=-Inf, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$during, xmax=end_phase3$during, ymin=-Inf, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$after, xmax=end_phase3$after, ymin=-Inf, ymax=Inf, fill=phase3.cols[3], alpha=.4)+
  annotate(geom="rect",xmin=begin_phase3$end, xmax=end_phase3$end, ymin=-Inf, ymax=Inf, fill="#bbbbbb", alpha=.4)+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  scale_color_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(gz.2,pz.2,cattle))+
  scale_fill_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(gz.2,pz.2,cattle))+
  scale_shape_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(16,17,15))+
  theme_classic()+
  labs(x="Date of observation",y="% Groups in habitat",fill="Species",color="Species",shape="Species")+ # Individuals doing activity
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "top"
  )+
  scale_y_continuous(labels = scales::percent, breaks=c(0,.25,.5,.75,1))+
  facet_wrap(~variable)



#---------------------------------------------------------------------------------------------#
#----------------------------------------% Bush Type 2----------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss$SpeciesName <- factor(find_replace(ss$Species,data.frame(x=c(zebra.abbr,"Cattle"),y=c(zebra.names,"Cattle"))), levels=c(zebra.names,"Cattle"))
order_bush <- c("X.open.grassland","X.light","X.medium","X.thick")
bush_names <- find_replace(order_bush,data.frame(x=order_bush,y=c("% Open grassland","% Light bush","% Medium bush","% Thick bush")))
mss <- melt(ss,measure.vars=order_bush)
mss$variable <- factor(mss$variable, levels=order_bush)
levels(mss$variable) <- bush_names

ggplot(filter(mss, Species%in%c(zebra.abbr,"Cattle"), variable%in%bush_names), aes(x=Days, y=value, color=variable, fill=variable, shape=variable))+
  annotate(geom="rect",xmin=begin_phase3$before, xmax=end_phase3$before, ymin=-Inf, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$during, xmax=end_phase3$during, ymin=-Inf, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$after, xmax=end_phase3$after, ymin=-Inf, ymax=Inf, fill=phase3.cols[3], alpha=.4)+
  annotate(geom="rect",xmin=begin_phase3$end, xmax=end_phase3$end, ymin=-Inf, ymax=Inf, fill="#bbbbbb", alpha=.4)+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  scale_color_manual(breaks=bush_names,labels=bush_names,values=c(og,lb,mb,tb))+
  scale_fill_manual(breaks=bush_names,labels=bush_names,values=c(og,lb,mb,tb))+
  scale_shape_manual(breaks=bush_names,labels=bush_names,values=c(16,17,15,23))+
  theme_classic()+
  labs(x="Date of observation",y="% Groups in habitat",fill="Habitat type",color="Habitat type",shape="Habitat type")+ # Individuals doing activity
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "top"
  )+
  # scale_y_continuous(labels = scales::percent, breaks=c(0,.25,.5,.75,1))+
  facet_wrap(~SpeciesName)


#---------------------------------------------------------------------------------------------#
#---------------------------------% Bush Type & Total cattle----------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss$SpeciesName <- factor(find_replace(ss$Species,data.frame(x=c(zebra.abbr,"Cattle"),y=c(zebra.names,"Cattle"))), levels=c(zebra.names,"Cattle"))
order_bush <- c("X.open.grassland","X.light","X.medium","X.thick")
bush_names <- find_replace(order_bush,data.frame(x=order_bush,y=c("% Open grassland","% Light bush","% Medium bush","% Thick bush")))
mss <- melt(ss,measure.vars=order_bush)
mss$variable <- factor(mss$variable, levels=order_bush)
levels(mss$variable) <- bush_names

cc <- filter(ss,Species=="Cattle")
cc$Mean.herd.size <- cc$Mean.herd.size/max(cc$Mean.herd.size)

ggplot(filter(mss, Species%in%c(zebra.abbr,"Cattle"), variable%in%bush_names), aes(x=Days, y=value, color=Species, fill=Species, shape=Species))+
  geom_point(data=cc, aes(x=Days,y=Mean.herd.size),size=3,fill="#333333",color="#333333",shape=18)+
  geom_point(data=cc, aes(x=Days,y=Mean.herd.size),size=3,fill="#333333",color="#333333",shape=5)+
  stat_smooth(data=cc, aes(x=Days,y=Mean.herd.size),method="lm",formula=y~poly(x,2))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  scale_color_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(gz.2,pz.2,cattle))+
  scale_fill_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(gz.2,pz.2,cattle))+
  scale_shape_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(16,17,15))+
  theme_classic()+
  labs(x="Date of observation",y="% Groups in habitat",fill="Species",color="Species",shape="Species")+ # Individuals doing activity
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "top"
  )+
  scale_y_continuous(labels = scales::percent, breaks=c(0,.25,.5,.75,1))+
  facet_wrap(~variable)


#---------------------------------------------------------------------------------------------#
#---------------------------------% Bush Type 2 & Total cattle--------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss$SpeciesName <- factor(find_replace(ss$Species,data.frame(x=c(zebra.abbr,"Cattle"),y=c(zebra.names,"Cattle"))), levels=c(zebra.names,"Cattle"))
order_bush <- c("X.open.grassland","X.light","X.medium","X.thick")
bush_names <- find_replace(order_bush,data.frame(x=order_bush,y=c("% Open grassland","% Light bush","% Medium bush","% Thick bush")))
mss <- melt(ss,measure.vars=order_bush)
mss$variable <- factor(mss$variable, levels=order_bush)
levels(mss$variable) <- bush_names

cc <- filter(ss,Species=="Cattle")
cc.1 <- cc
cc.2 <- cc
cc.1$SpeciesName <- "Grevy's zebras"
cc.2$SpeciesName <- "Plains zebras"
cc <- rbind(cc.1,cc.2)
cc$Mean.herd.size <- cc$Mean.herd.size/max(cc$Mean.herd.size)
cc$variable <- "Cattle"

ggplot(filter(mss, Species%in%c(zebra.abbr), variable%in%bush_names[2:3]), aes(x=Days, y=value, color=variable, fill=variable, shape=variable))+
  geom_point(data=cc, aes(x=Days,y=Mean.herd.size),size=3,fill="#333333",color="#333333",shape=18)+
  geom_point(data=cc, aes(x=Days,y=Mean.herd.size),size=3,fill="#333333",color="#333333",shape=5)+
  stat_smooth(data=cc, aes(x=Days,y=Mean.herd.size),method="lm",formula=y~poly(x,2))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  scale_color_manual(breaks=bush_names[2:3],labels=bush_names[2:3],values=c(c(og,lb,mb,tb)[2:3],"#333333"))+
  scale_fill_manual(breaks=bush_names[2:3],labels=bush_names[2:3],values=c(c(og,lb,mb,tb)[2:3],"#333333"))+
  scale_shape_manual(breaks=bush_names[2:3],labels=bush_names[2:3],values=c(c(16,17,15,23)[2:3],18))+
  theme_classic()+
  labs(x="Date of observation",y="% Groups in habitat",fill="Habitat type",color="Habitat type",shape="Habitat type")+ # Individuals doing activity
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "top"
  )+
  scale_y_continuous(labels = scales::percent, breaks=c(0,.25,.5,.75,1))+
  facet_wrap(~SpeciesName)


#---------------------------------------------------------------------------------------------#
#---------------------------------% Bush Type ~ Total cattle--------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss$SpeciesName <- factor(find_replace(ss$Species,data.frame(x=c(zebra.abbr,"Cattle"),y=c(zebra.names,"Cattle"))), levels=c(zebra.names,"Cattle"))
order_bush <- c("X.open.grassland","X.light","X.medium","X.thick")
bush_names <- find_replace(order_bush,data.frame(x=order_bush,y=c("% Open grassland","% Light bush","% Medium bush","% Thick bush")))
mss <- melt(ss,measure.vars=order_bush)
mss$variable <- factor(mss$variable, levels=order_bush)
levels(mss$variable) <- bush_names

cc <- filter(ss,Species=="Cattle")
cc.1 <- cc
cc.2 <- cc
cc.1$SpeciesName <- "Grevy's zebras"
cc.2$SpeciesName <- "Plains zebras"
cc <- rbind(cc.1,cc.2)
cc$Mean.herd.size <- cc$Mean.herd.size/max(cc$Mean.herd.size)
cc$variable <- "Cattle"

ggplot(filter(mss, Species%in%c(zebra.abbr), variable%in%bush_names[2:3]), aes(x=Total.cattle, y=value, color=variable, fill=variable, shape=variable))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  scale_color_manual(breaks=bush_names[2:3],labels=bush_names[2:3],values=c(c(og,lb,mb,tb)[2:3],"#333333"))+
  scale_fill_manual(breaks=bush_names[2:3],labels=bush_names[2:3],values=c(c(og,lb,mb,tb)[2:3],"#333333"))+
  scale_shape_manual(breaks=bush_names[2:3],labels=bush_names[2:3],values=c(c(16,17,15,23)[2:3],18))+
  theme_classic()+theme(legend.position="top")+
  labs(x="Total cattle present",y="% Groups in habitat",fill="Habitat type",color="Habitat type",shape="Habitat type")+ # Individuals doing activity
  scale_y_continuous(labels = scales::percent, breaks=c(0,.25,.5,.75,1))+
  facet_wrap(~SpeciesName+variable)



#---------------------------------------------------------------------------------------------#
#---------------------------% Bush Type & Total cattle Analysis-------------------------------#
#---------------------------------------------------------------------------------------------#

Anova(aov(X.light ~ Species+Total.cattle+Exp.1,ss),type="II")
lm()



#---------------------------------------------------------------------------------------------#
#-----------------------------------------% Combined------------------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss$SpeciesName <- factor(find_replace(ss$Species,data.frame(x=c(zebra.abbr,"Cattle"),y=c(zebra.names,"Cattle"))), levels=c(zebra.names,"Cattle"))

ggplot(filter(ss, Species%in%zebra.abbr), aes(x=Days, y=X.Combined, color=SpeciesName, fill=SpeciesName, shape=SpeciesName))+
  annotate(geom="rect",xmin=begin_phase3$before, xmax=end_phase3$before, ymin=-Inf, ymax=Inf, fill=phase3.cols[1], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$during, xmax=end_phase3$during, ymin=-Inf, ymax=Inf, fill=phase3.cols[2], alpha=.5)+
  annotate(geom="rect",xmin=begin_phase3$after, xmax=end_phase3$after, ymin=-Inf, ymax=Inf, fill=phase3.cols[3], alpha=.4)+
  annotate(geom="rect",xmin=begin_phase3$end, xmax=end_phase3$end, ymin=-Inf, ymax=Inf, fill="#bbbbbb", alpha=.4)+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  scale_color_manual(breaks=zebra.names,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_fill_manual(breaks=zebra.names,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.names,labels=zebra.names,values=c(16,17,15,23)[2:3])+
  theme_classic()+
  labs(x="Date of observation",y="% Groups combined",fill="Species",color="Species",shape="Species")+ # Individuals doing activity
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "top"
  )+
  scale_y_continuous(labels = scales::percent, breaks=c(0,.25,.5,.75,1))+
  facet_wrap(~SpeciesName)

#---------------------------------------------------------------------------------------------#
#-----------------------------------% Combined & Total cattle---------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss$SpeciesName <- factor(find_replace(ss$Species,data.frame(x=c(zebra.abbr,"Cattle"),y=c(zebra.names,"Cattle"))), levels=c(zebra.names,"Cattle"))

cc <- filter(ss,Species=="Cattle")
cc.1 <- cc
cc.1$SpeciesName <- zebra.names[1]
cc.2 <- cc
cc.2$SpeciesName <- zebra.names[2]
cc <- rbind(cc.1,cc.2)
cc$Mean.herd.size <- cc$Mean.herd.size/max(cc$Mean.herd.size)

ggplot(filter(ss, Species%in%zebra.abbr), aes(x=Days, y=X.Combined, color=Species, fill=Species, shape=Species))+
  geom_point(data=cc,aes(x=Days,y=Mean.herd.size),size=3,fill="#333333",color="#333333",shape=18)+
  geom_point(data=cc,aes(x=Days,y=Mean.herd.size),size=3,fill="#333333",color="#333333",shape=5)+
  stat_smooth(data=cc,aes(x=Days,y=Mean.herd.size),method="lm",formula=y~poly(x,2))+#,color="#333333",fill="#333333")+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  scale_color_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(gz.2,pz.2,"#333333"))+
  scale_fill_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(gz.2,pz.2,"#333333"))+
  scale_shape_manual(breaks=c(zebra.abbr,"Cattle"),labels=c(zebra.names,"Cattle"),values=c(c(16,17,15,23)[2:3],18))+
  theme_classic()+
  labs(x="Date of observation",y="% Groups combined",fill="Species",color="Species",shape="Species")+
  scale_x_continuous(breaks=x_days,labels=x_days_value)+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=-45,
      vjust=0
    ),
    legend.position = "none"
  )+
  scale_y_continuous(labels = scales::percent, breaks=c(0,.25,.5,.75,1))+
  facet_wrap(~SpeciesName)


#---------------------------------------------------------------------------------------------#
#-----------------------------------% Combined ~ Total cattle---------------------------------#
#---------------------------------------------------------------------------------------------#
source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
ss$SpeciesName <- factor(find_replace(ss$Species,data.frame(x=c(zebra.abbr,"Cattle"),y=c(zebra.names,"Cattle"))), levels=c(zebra.names,"Cattle"))

ggplot(filter(ss, Species%in%c(zebra.abbr)), aes(x=Total.cattle, y=X.Combined, color=SpeciesName, fill=SpeciesName, shape=SpeciesName))+
  geom_point(size=3)+
  stat_smooth(method="lm",formula=y~poly(x,2))+
  scale_color_manual(breaks=zebra.names,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_fill_manual(breaks=zebra.names,labels=zebra.names,values=c(gz.2,pz.2))+
  scale_shape_manual(breaks=zebra.names,labels=zebra.names,values=c(16,17,15,23)[2:3])+
  theme_classic()+theme(legend.position="top")+
  labs(x="Total cattle present",y="% Groups combined",fill="Species",color="Species",shape="Species")+
  scale_y_continuous(labels = scales::percent, breaks=c(0,.25,.5,.75,1))+
  facet_wrap(~SpeciesName)




#---------------------------------------------------------------------------------------------#
#-----------------------------------% Combined analysis---------------------------------------#
#---------------------------------------------------------------------------------------------#
kruskal.test(X.Combined~Exp.1,filter(ss,Species=="GZ",Exp.1!="Not in experiment"))
kruskal.test(X.Combined~Exp.1,filter(ss,Species=="PZ",Exp.1!="Not in experiment"))

zebra <- ss %>% filter(Species=="GZ", !is.na(X.Combined), !is.na(Exp.1), Exp.1!="Not in experiment")
kruskal.test(X.Combined ~ Exp.1, zebra)
m <- matrix(1, length(unique(zebra$Exp.1)), length(unique(zebra$Exp.1)))
n <- matrix("", length(unique(zebra$Exp.1)), length(unique(zebra$Exp.1)))
for (i in 1:length(unique(zebra$Exp.1))) {
  for (j in (i+1):length(unique(zebra$Exp.1))) {
    if (j > length(unique(zebra$Exp.1))) break
    two.groups <- unique(zebra$Exp.1)[c(i,j)]
    p <- wilcox.test(X.Combined ~ Exp.1, zebra %>% filter(Exp.1 %in% two.groups))$p.value
    cat(two.groups,"p=",p,find_replace((p<=0.05), data.frame(x=c(T,F),y=c("*",""))),"\n")
    m[i,j] <- m[j,i] <- p
    if (p <= 0.05) n[i,j] <- n[j,i] <- round(p,4)
  }
}
rownames(m) <- colnames(m) <- rownames(n) <- colnames(n) <- unique(zebra$Exp.1)
View(m)
View(n)




#################################################################################################
##---------------------------------------------------------------------------------------------##
##----------------------------------------Archive----------------------------------------------##
##---------------------------------------------------------------------------------------------##
#################################################################################################