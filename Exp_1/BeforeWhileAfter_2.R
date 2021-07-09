rm(list=ls())

source('~/Desktop/MPALA/mpala.R')
source('~/Desktop/MPALA/Analysis/AddMobSizeCategorical.R')
df <- filter(df, is.before(mdy(df$Date),ymd("2021-07-05")), !(Identifier%in%blacklist), Species%in%zebra.abbr)

mean.lat <- mean(filter(df, is.before(mdy(df$Date),ymd("2021-06-14")))$Latitude)
mean.long <- mean(filter(df, is.before(mdy(df$Date),ymd("2021-06-14")))$Longitude)

df$Distance.from.origin <- sqrt((df$Longitude-mean.long)^2+(df$Latitude-mean.lat)^2)
df$Angle.from.origin <- atan((df$Latitude-mean.lat)/(df$Longitude-mean.long+1e-12))

df.before <- filter(df, is.before(mdy(df$Date),ymd("2021-06-14")))
df.while <- filter(df, is.strictly.after(mdy(df$Date),ymd("2021-06-14")), is.strictly.before(mdy(df$Date),ymd("2021-06-19")))
df.after <- filter(df, is.after(mdy(df$Date),ymd("2021-06-19")))
df.before$Exp.1 <- "before"
df.while$Exp.1 <- "while"
df.after$Exp.1 <- "after"
df.exp.1 <- rbind(df.before,df.while,df.after)
df.exp.1$Exp.1 <- factor(df.exp.1$Exp.1, levels=c("before","while","after"))
levels(df.exp.1$Exp.1) <- c("Before","While","After")

# df.before <- filter(df, is.strictly.before(mdy(df$Date),ymd("2021-06-18")))
# df.after <- filter(df, is.after(mdy(df$Date),ymd("2021-06-18")))
# df.before$Exp.1 <- "before"
# df.after$Exp.1 <- "after"
# df.exp.1 <- rbind(df.before,df.after)
# df.exp.1$Exp.1 <- factor(df.exp.1$Exp.1, levels=c("before","after"))
# levels(df.exp.1$Exp.1) <- c("Before","After")


df.exp.1$Tree.cover <- find_replace(df.exp.1$Primary.habitat,data.frame(x=bushland,y=c(0.1,0.5,0.9)))
df.exp.1$Secondary.tree.cover <- find_replace(df.exp.1$Secondary.habitat,data.frame(x=bushland,y=c(0.1,0.5,0.9)))
df.exp.1$Tertiary.tree.cover <- find_replace(df.exp.1$Tertiary.habitat,data.frame(x=bushland,y=c(0.1,0.5,0.9)))

df.exp.1$Factory <- paste(df.exp.1$Exp.1,df.exp.1$Species,sep="_")
my.nova(df.exp.1, "Total.animals", "Factory")

mean_plus_sd <- function(vec) mean(vec)+sd(vec)
mean_minus_sd <- function(vec) mean(vec)-sd(vec)

dodge <- position_dodge(width = 1)
ggplot(data = filter(df.exp.1, !is.na(Total.animals)), aes(x=Species, y=Total.animals, fill=Exp.1)) +
  geom_violin(position = dodge, trim=T)+
  geom_boxplot(width=.1, outlier.colour=NA, position = dodge)+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black", aes(g=Exp.1), position=dodge)+
  stat_summary(fun=mean_plus_sd, geom="point", shape=1, size=3, fill="black", aes(g=Exp.1), position=dodge)+
  stat_summary(fun=mean_minus_sd, geom="point", shape=1, size=3, fill="black", aes(g=Exp.1), position=dodge)+
  theme_classic()+
  scale_x_discrete(limits=zebra.abbr,labels=zebra.names)+
  scale_fill_manual(labels=c("Before phalanx arrival","As phalanx arrive","After phalanx arrival"), values=c(pastel.pink,pastel.yellow,pastel.green))+
  labs(x="Species",y="Dazzle size (individuals)",fill="Experiment phase")+
  theme(legend.position="top")



ggplot(filter(df.exp.1, !is.na(Mob.size.categorical)), aes(x=Exp.1, y=Total.animals, fill=Mob.size.categorical))+
  geom_violin(trim=FALSE, position = dodge)+
  geom_boxplot(width=0.1, outlier.colour=NA, position = dodge)+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black", aes(g=Mob.size.categorical), position=dodge)+
  theme_classic()


+
  scale_x_discrete(limits=group_order,labels=group_limits)+
  scale_color_manual(name=group_name,values=colours[1:length(group_order)])+
  scale_fill_manual(labels=group_order, values=colours[1:length(group_order)])+
  labs(x=group_name,y=variable_name)+
  theme(legend.position="none")







