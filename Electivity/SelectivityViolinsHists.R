rm(list=ls())

library(ggplot2)

df <- read.csv("/Users/maxgotts/Desktop/MPALA/Whitesheets/ConvertedWhitesheets.csv")

# Colours
red <- "#fc1303"
orange <- "#fc8403"
yellow <- "#f7d114"
green <- "#00c750"
blue <- "#033dfc" #1644db

ggplot(df, aes(QuickSpecies, Primary.habitat, fill=QuickSpecies))+
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1)+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black")+
  theme_bw()+
  scale_color_manual(name="Species",values=c(red,orange,yellow,green))+
  scale_fill_manual(name="Species",values=c(red,orange,yellow,green))+
  scale_x_discrete(limits=c("PZ","GZ","Cattle","Camel"),labels=c("Plains zebra","Grevy's zebra","Cattle","Camel"))+
  labs(x="Species",y="Primary habitat")+
  scale_y_discrete(limits=as.factor(c(1,2,3)),labels=c("Open bush","Light bush","Medium bush"))+
  theme(legend.position="none")

df <- filter(df, Distance.secondary <=0.01)

ggplot(df, aes(QuickSpecies, Distance.secondary, fill=QuickSpecies))+
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1)+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black")+
  theme_bw()+
  scale_color_manual(name="Species",values=c(red,orange,yellow,green))+
  scale_fill_manual(name="Species",values=c(red,orange,yellow,green))+
  scale_x_discrete(limits=c("Plains zebra","Grevy's zebra","Cattle","Camel"),labels=c("Plains zebra","Grevy's zebra","Cattle","Camel"))+
  labs(x="Species",y="Distance to secondary habitat (degrees)")+
  theme(legend.position="none")
