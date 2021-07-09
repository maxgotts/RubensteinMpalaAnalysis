rm(list=ls())
source('~/Desktop/MPALA/mpala.R')

# df <- filter(df, is.before(mdy(Date),ymd("2021-06-10")))
df.z <- filter(df, Species%in%zebra.abbr)#, Activity=="Drinking")
ggplot(df.z, aes(x=Identifier, y=NDVI,fill=QuickSpecies))+
  geom_col(color=NA,position="dodge")+
  # facet_wrap(~QuickSpecies)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90),
        legend.position="top")


summary(anova <- aov(NDVI ~ Activity, df.z))
TukeyHSD(anova)
kruskal.test(NDVI ~ Activity, df.z)

