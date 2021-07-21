rm(list=ls())

library(scales)

source('~/Desktop/MPALA/mpala.R')
source('~/Desktop/MPALA/Analysis/Exp_1/Experiment_1.R')
hb <- read.csv("/Users/maxgotts/Desktop/MPALA/Maps/TIFFs.csv")

# df <- filter(df, Species%in%zebra.abbr)
df$Group <- find_replace(df$QuickSpecies, data.frame(x=c(zebra.abbr,"Cattle"), y=c(zebra.names,"Cattle")))
df <- df[,c("Date","NDVI","Group","Exp.1")]

number.points <- 1000
NDVI_1 <- colnames(hb)[4]
NDVI_2 <- colnames(hb)[6]
NDVI_3 <- colnames(hb)[8]
frame1 <- hb %>% filter(!is.na(NDVI_1)) %>% sample_n(number.points)
frame1$Date <- paste0(vi_dates$start[1],"-2021")
frame1$NDVI <- frame1[,NDVI_1]
frame1$Exp.1 <- "Before"
frame2 <- hb  %>% filter(!is.na(NDVI_2)) %>% sample_n(number.points)
frame2$Date <- paste0(vi_dates$start[2],"-2021")
frame2$NDVI <- frame2[,NDVI_2]
frame2$Exp.1 <- "While"
frame3 <- hb  %>% filter(!is.na(NDVI_3)) %>% sample_n(number.points)
frame3$Date <- paste0(vi_dates$start[3],"-2021")
frame3$NDVI <- frame3[,NDVI_3]
frame3$Exp.1 <- "After"
frames <- rbind(frame1,frame2,frame3)
frames$Group <- "Random"
frames <- frames[,c("Date","NDVI","Group","Exp.1")]

ndvi <- rbind(frames,df)

ndvi$Group <- factor(ndvi$Group, levels=c(zebra.names,"Cattle","Random"))
ndvi <- filter(ndvi, !is.na(Group), NDVI<=.6, NDVI>=0.15)
ggplot(ndvi, aes(x=mdy(Date), y=NDVI, color=Group))+
  
  
  geom_point(size=3)+
  stat_smooth(method="lm", formula=y~x)+
  labs(x="Date",y="NDVI",color="Source of NDVI")+
  scale_x_date(date_breaks = "days", labels = date_format("%d-%b"))+
  scale_color_manual(values=c(gz.2,pz.2,cattle,camel))+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=90,
      vjust=0
    )
  )

summary(lm(NDVI ~ mdy(Date), ndvi %>% filter(Group==unique(ndvi$Group)[3], !is.na(NDVI))))


# Anova(aov(NDVI ~ , ndvi), Type="II")


  # geom_text(x=mdy("6/4/21"),y=max(ndvi$NDVI), label=phase3.names[1],color="black", size=100)
