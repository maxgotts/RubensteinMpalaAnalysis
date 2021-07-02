library(dplyr)

df <- read.csv("/Users/maxgotts/Desktop/MPALA/Whitesheets/ConvertedWhitesheets.csv")

# Z <- filter(df, QuickSpecies%in%c("GZ","PZ"))
Z <- filter(df, QuickSpecies=="GZ")
(hab.zeb <- kruskal.test(Total.animals ~ Primary.habitat, data=Z))
TukeyHSD(hab.zeb)

aov(Length ~ Group, data = df)


# Colours
red <- "#fc1303"
orange <- "#fc8403"
yellow <- "#f7d114"
green <- "#00c750"
blue <- "#033dfc" #1644db

ggplot(df, aes(x=Secondary.habitat, y=Total.animals, fill=Secondary.habitat))+
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1)+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black")+
  theme_classic()+
  scale_color_manual(name="Habitat type",values=c(red,orange,yellow))+
  scale_fill_manual(name="Habitat type",values=c(red,orange,yellow))+
  scale_x_discrete(limits=c("OB","LB","MB"),labels=c("Open bush","Light bush","Medium bush"))+
  labs(x="Habitat type",y="Total animals")+
  theme(legend.position="none")





