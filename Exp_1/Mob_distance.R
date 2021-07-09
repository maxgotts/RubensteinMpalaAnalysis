rm(list=ls())

source('~/Desktop/MPALA/mpala.R')

df <- filter(df, !(Identifier%in%blacklist), !is.na(Distance.to.mob), !is.na(Closest.mob.size), is.before(mdy(df$Date),ymd("2021-07-05")))
df.mob <- df[,c("Identifier", "Date", "Latitude", "Longitude", "Species", "Total.animals", "Distance.to.mob", "Closest.mob.size")]

df.mob <- duplicate.rows(df.mob)
df.mob$Latitude <- as.numeric(df.mob$Latitude)
df.mob$Longitude <- as.numeric(df.mob$Longitude)
df.mob$Total.animals <- as.numeric(df.mob$Total.animals)
df.mob$Distance.to.mob <- as.numeric(df.mob$Distance.to.mob)
df.mob$Closest.mob.size <- as.numeric(df.mob$Closest.mob.size)

df.mob$Mob.size.categorical <- NA
df.mob[df$Closest.mob.size<=mean(df$Closest.mob.size),"Mob.size.categorical"] <- "Small"
df.mob[df$Closest.mob.size>mean(df$Closest.mob.size),"Mob.size.categorical"] <- "Large"

df.mob$Phase <- NA
df.mob[is.strictly.before(mdy(df$Date),ymd("2021-06-18")),"Phase"] <- "Before"
df.mob[is.after(mdy(df$Date),ymd("2021-06-18")),"Phase"] <- "After"

df.mob$Phase.2 <- NA
df.mob[is.before(mdy(df$Date),ymd("2021-06-14")),"Phase.2"] <- "Before"
df.mob[is.strictly.after(mdy(df$Date),ymd("2021-06-14"))&is.strictly.before(mdy(df$Date),ymd("2021-06-19")),"Phase.2"] <- "While"
df.mob[is.after(mdy(df$Date),ymd("2021-06-19")),"Phase.2"] <- "After"

df.mob$Phase <- factor(df.mob$Phase, levels=c("Before","After"))
df.mob$Phase.2 <- factor(df.mob$Phase.2, levels=c("Before","While","After"))

## Plot constants
Phase.cols <- c(pastel.pink, pastel.green) #c(while.col, after.col)
Phase.2.cols <- c(pastel.pink,pastel.yellow,pastel.green) #c(before.col, while.col, after.col)

## Violin plots
dodge <- position_dodge(width = 1)

ggplot(data = df.mob, aes(x = Mob.size.categorical, y = Distance.to.mob, fill = Phase)) +
  geom_violin(position = dodge, trim=T)+
  geom_boxplot(width=.1, outlier.colour=NA, position = dodge)+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black", aes(g=Phase), position=dodge)+ #aes(fill=Phase)
  theme_classic()+
  scale_x_discrete(limits=c("Small","Large"),labels=c("Small mob","Large mob"))+
  scale_fill_manual(labels=c("Before phalanx arrival","After phalanx arrival"), values=Phase.cols)+
  labs(x="Mob size",y="Distance to closest mob (m)",fill="Experiment phase")+
  theme(legend.position="top")

ggplot(data = df.mob, aes(x = Mob.size.categorical, y = Distance.to.mob, fill = Phase.2)) +
  geom_violin(position = dodge, trim=T)+
  geom_boxplot(width=.1, outlier.colour=NA, position = dodge)+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black", aes(g=Phase.2), position=dodge)+ #aes(fill=Phase)
  theme_classic()+
  scale_x_discrete(limits=c("Small","Large"),labels=c("Small mob","Large mob"))+
  scale_fill_manual(labels=c("Before phalanx arrival","As phalanx arrive","After phalanx arrival"), values=Phase.2.cols)+
  labs(x="Mob size",y="Distance to closest mob (m)",fill="Experiment phase")+
  theme(legend.position="top")

# filter(df.mob, Mob.size.categorical=="Large", Phase.2=="While")


## Regression plots
ggplot(data = df.mob, aes(x=Closest.mob.size, y=Distance.to.mob, color=Phase))+
  geom_point()+
  geom_smooth(method='lm', formula=y~poly(x,2))+
  scale_color_manual(labels=c("Before phalanx arrival","After phalanx arrival"), values=Phase.cols)+
  labs(x="Mob size (individuals)",y="Distance to closest mob (m)",fill="Experiment phase")+
  theme(legend.position="top")+theme_classic()

summary(lm(Distance.to.mob~Closest.mob.size, filter(df.mob, Phase=="Before")))
summary(lm(Distance.to.mob~Closest.mob.size, filter(df.mob, Phase=="After")))

summary(lm(Distance.to.mob~poly(Closest.mob.size,2), filter(df.mob, Phase=="Before")))
summary(lm(Distance.to.mob~poly(Closest.mob.size,2), filter(df.mob, Phase=="After")))

ggplot(data = df.mob, aes(x=Closest.mob.size, y=Distance.to.mob, color=Phase.2))+
  geom_point()+
  geom_smooth(method='lm', formula=y~poly(x,2))+
  scale_color_manual(labels=c("Before phalanx arrival","As phalanx arrive","After phalanx arrival"), values=Phase.2.cols)+
  labs(x="Mob size (individuals)",y="Distance to closest mob (m)",fill="Experiment phase")+
  theme(legend.position="top")+theme_classic()

summary(lm(Distance.to.mob~Closest.mob.size, filter(df.mob, Phase.2=="Before")))
summary(lm(Distance.to.mob~Closest.mob.size, filter(df.mob, Phase.2=="While")))
summary(lm(Distance.to.mob~Closest.mob.size, filter(df.mob, Phase.2=="After")))

summary(lm(Distance.to.mob~poly(Closest.mob.size,2), filter(df.mob, Phase.2=="Before")))
summary(lm(Distance.to.mob~poly(Closest.mob.size,2), filter(df.mob, Phase.2=="While")))
summary(lm(Distance.to.mob~poly(Closest.mob.size,2), filter(df.mob, Phase.2=="After")))

summary(lm(Distance.to.mob~poly(Closest.mob.size,2), df.mob %>% sample_n(round(nrow(df.mob)*0.75))))


## Tests
df.mob$Combined.factor <- paste0(df.mob$Mob.size.categorical,"_",df.mob$Phase)
df.mob$Combined.factor.2 <- paste0(df.mob$Mob.size.categorical,"_",df.mob$Phase.2)

# ANOVA
leveneTest(Distance.to.mob ~ Combined.factor, df.mob)
my.nova(df.mob, "Distance.to.mob", "Combined.factor",group_settings=list(valid=2))   # SW 2.02e-09, LT 0.05416, ANOVA 0.00471, KW 0.007604
                                                                                     # Large_Before-Large_After 0.0177373
                                                                                     # Small_Before-Large_Before 0.0143575
leveneTest(Distance.to.mob ~ Combined.factor.2, df.mob)
my.nova(df.mob, "Distance.to.mob", "Combined.factor.2",group_settings=list(valid=2)) # SW 2.02e-09, LT 0.0006498, ANOVA 0.0619, KW 0.2641

leveneTest(Distance.to.mob~Mob.size.categorical,df.mob)
leveneTest(Distance.to.mob~Phase,df.mob)
ancova <- aov(Distance.to.mob~Mob.size.categorical+Phase,df.mob)
Anova(ancova,type="III")

leveneTest(Distance.to.mob~Mob.size.categorical*Phase,df.mob)
ancova <- aov(Distance.to.mob~Mob.size.categorical*Phase,df.mob)
Anova(ancova,type="III")



leveneTest(Distance.to.mob~Mob.size.categorical,df.mob)
leveneTest(Distance.to.mob~Phase.2,df.mob)
ancova <- aov(Distance.to.mob~Mob.size.categorical+Phase.2,df.mob)
Anova(ancova,type="III")

leveneTest(Distance.to.mob~Mob.size.categorical*Phase.2,df.mob)
ancova <- aov(Distance.to.mob~Mob.size.categorical*Phase.2,df.mob)
Anova(ancova,type="III")



## General visualization
ggplot(df.mob, aes(x=mdy(Date), y=Total.animals, color=log10(Distance.to.mob), shape=Phase.2))+
  geom_point(size=3)+
  labs(x="Date",y="Mob size (individuals)",color="Distance to mob (m)", shape="Phase of experiment")+
  scale_colour_gradient(
    low = "#ec4176", #"#ffa45e", #"#B06565", #"#114b5f",
    high = "#262254", #"#9a77cf", #"#242F50", #"#c6dabf",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour",
    breaks = log10(c(250,500,1000,2000,4000)),
    labels = c(250,500,1000,2000,4000)
  )+
  scale_x_date(date_breaks = "days", labels = date_format("%d-%b"))+
  theme_classic()+theme(
    axis.text.x = element_text(
      size=10,
      angle=90,
      vjust=0
    )
  )

