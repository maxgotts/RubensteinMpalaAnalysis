rm(list=ls())

library(dplyr)
library(ggplot2)
source('~/Desktop/MPALA/mpala.R')


hb <- read.csv("/Users/maxgotts/Desktop/MPALA/Maps/Habitat/Habitat.csv")
# MPALA/Maps/MODUS Data/6-3-6-18/MODUS_NDVI_6-3-6-18


shapiro.test((hb %>% sample_n(5000))$NDVI)
(groups.compare <- kruskal.test(NDVI ~ Habitat, data=hb)) #aov #kruskal.test
TukeyHSD(aov(NDVI ~ Habitat, data=hb))


hb$Habitat.factor <- factor(hb$Habitat, levels=bushland)
ggplot(hb, aes(x=Habitat.factor, y=NDVI, fill=Habitat.factor))+
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1)+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black")+
  theme_bw()+
  scale_color_discrete(breaks=bushland,labels=bush.names)+
  scale_fill_manual(values=c(og,lb,mb))+
  scale_x_discrete(limits=bushland,labels=bush.names)+
  labs(x="Habitat type",y="NDVI")+
  theme(legend.position="none")


ggplot(hb, aes(x=NDVI))+
  geom_histogram(fill=mb,color="black",binwidth=0.01)+
  theme_classic()

shapiro.test((hb %>% sample_n(5000))$GrassNDVI)
(groups.compare <- kruskal.test(GrassNDVI ~ Habitat, data=hb)) #aov #kruskal.test
TukeyHSD(aov(GrassNDVI ~ Habitat, data=hb))


hb$Habitat.factor <- factor(hb$Habitat, levels=bushland)
ggplot(hb, aes(x=Habitat.factor, y=GrassNDVI, fill=Habitat.factor))+
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1)+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black")+
  theme_bw()+
  scale_color_discrete(breaks=bushland,labels=bush.names)+
  scale_fill_manual(values=c(ob,lb,mb))+
  scale_x_discrete(limits=bushland,labels=bush.names)+
  labs(x="Habitat type",y="Grass NDVI")+
  theme(legend.position="none")
