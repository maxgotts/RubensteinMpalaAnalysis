rm(list=ls())

source('~/Desktop/MPALA/mpala.R')

ggplot(df, aes(x=mdy(Date), y=Hour+Minute/60, color=Loop,fill=Loop,shape=Loop))+
  geom_point(size=3)+
  labs(x="Date",y="Hour",color="Loop")+
  lims(y=c(8,16))+
  scale_x_date(date_breaks = "days", labels = date_format("%d-%b"))+
  scale_color_manual(values=c(red,orange,yellow,green,blue))+
  scale_fill_manual(values=c(red,orange,yellow,green,blue))+
  scale_shape_manual(values=c(16,17,15,23,25))+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=90,
      vjust=0
    )
  )
