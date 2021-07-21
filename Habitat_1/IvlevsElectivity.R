rm(list=ls())

library(ggplot2)
library(dplyr)
library(reshape2)

source('~/Desktop/MPALA/mpala.R')

df <- read.csv("/Users/maxgotts/Desktop/MPALA/Whitesheets/ConvertedWhitesheets.csv")
df <- filter(df, !is.na(Primary.habitat), !is.na(Total.animals), !is.na(Species))

Habitat <- read.csv("/Users/maxgotts/Desktop/MPALA/Maps/TIFFs.csv")


# Colours
red <- "#fc1303"
orange <- "#fc8403"
yellow <- "#f7d114"
green <- "#00c750"
blue <- "#033dfc" #1644db
og <- "#ffcc33"
lb <- "#a3c586"
mb <- "#5b7444"
gz <- "#a02923"
pz <- "#fdfdf6"
cattle <- "#aaaaaa"
camel <- "#333333"


## Electivity
electivity <- as.data.frame(matrix(NA,length(unique(df$QuickSpecies)),length(unique(df$Primary.habitat))))
rownames(electivity) <- unique(df$QuickSpecies)
colnames(electivity) <- unique(df$Primary.habitat)
for (qs in unique(df$QuickSpecies)) {
  for (hab in unique(df$Primary.habitat)) {
    df.qs <- filter(df, QuickSpecies==qs)
    df.qs.hab <- filter(df, QuickSpecies==qs, Primary.habitat==hab)
    df.hab <- filter(Habitat, Habitat==hab)
    r <- sum(df.qs.hab$Total.animals)/sum(df.qs$Total.animals)
    p <- nrow(df.hab)/nrow(Habitat)
    electivity[[qs,hab]] <- (r-p)/(r+p)
  }
}
View(electivity)

not_abs_bold <- function(V,a) {
  V[abs(V)>=a] <- ""
  return(V)
}
abs_bold <- function(V,a) {
  V[abs(V)<a] <- ""
  return(V)
}
electivity$QuickSpecies <- rownames(electivity)
melectivity <- melt(electivity)
colnames(melectivity) <- c("QuickSpecies","Habitat","E")
melectivity$Habitat <- factor(melectivity$Habitat, levels=c("OG","LB","MB"))

ggplot(melectivity, aes(x=QuickSpecies, y=E, fill=Habitat))+
  annotate(geom="rect",xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, fill=gz.2, alpha=.4)+
  annotate(geom="rect",xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, fill=pz.2, alpha=.4)+
  annotate(geom="rect",xmin=2.5, xmax=3.5, ymin=-Inf, ymax=Inf, fill=cattle, alpha=.4)+
  theme_classic()+
  geom_col(position="dodge")+
  scale_fill_manual(labels=c("Open bush","Light bush","Medium bush"), values=c(og,lb,mb))+
  labs(x="Species",y="Ivlev's Electivity Index (E')",fill="Habitat")+
  scale_x_discrete(limits=c("GZ","PZ","Cattle"),labels=c("Grevy's zebra","Plains zebra","Cattle"))+
  theme(legend.position="top")+
  geom_text(aes(label = abs_bold(round(E,3),0.1), y = E + 0.0365*(sign(E)-1), fontface=2), position = position_dodge(1), vjust = -0.5,family="Arial")+
  geom_text(aes(label = not_abs_bold(round(E,3),0.1), y = E + 0.0365*(sign(E)-1)), position = position_dodge(1), vjust = -0.5,family="Arial")+
  # geom_ribbon(aes(xmin=-Inf, xmax=Inf), ymin=-0.1, ymax=0.1))#, colour="grey", fill="grey", linetype="dashed", alpha = 1)
  geom_rect(aes(xmin=0, xmax=Inf, ymin=-0.1, ymax=0.1), colour=NA, fill="grey", linetype="dashed", alpha = 0.04)
  # geom_hline(aes(yintercept=0.1,color="black", linetype="dashed"))+geom_hline(aes(yintercept=-0.1,color="black", linetype="dashed"))

melectivity$QuickSpecies <- factor(melectivity$QuickSpecies, levels=c("GZ","PZ","Cattle","Camel"))
ggplot(melectivity, aes(x=Habitat, y=E, fill=QuickSpecies))+
  theme_classic()+
  geom_col(position="dodge")+
  scale_fill_manual(labels=c("Grevy's zebra","Plains zebra","Cattle","Camel"), values=c(gz,pz,cattle,camel))+
  labs(x="Species",y="Ivlev's Electivity Index (E')",fill="Habitat")+
  scale_x_discrete(limits=c("OB","LB","MB"),labels=c("Open bush","Light bush","Medium bush"))+
  theme(legend.position="top")+
  geom_text(aes(label = abs_bold(round(E,3),0.1), y = E + 0.0365*(sign(E)-1), fontface=2), position = position_dodge(1), vjust = -0.5,family="Arial", size=3.3)+
  geom_text(aes(label = not_abs_bold(round(E,3),0.1), y = E + 0.0365*(sign(E)-1)), position = position_dodge(1), vjust = -0.5,family="Arial",size=3.3)+
  # geom_ribbon(aes(xmin=-Inf, xmax=Inf), ymin=-0.1, ymax=0.1))#, colour="grey", fill="grey", linetype="dashed", alpha = 1)
  geom_rect(aes(xmin=0, xmax=Inf, ymin=-0.1, ymax=0.1), colour=NA, fill="grey", linetype="dashed", alpha = 0.04)
# geom_hline(aes(yintercept=0.1,color="black", linetype="dashed"))+geom_hline(aes(yintercept=-0.1,color="black", linetype="dashed"))


shapiro.test(df$Total.animals)
summary(anova <- aov(Total.animals ~ QuickSpecies, df))
kruskal.test(Total.animals ~ QuickSpecies, df)
TukeyHSD(anova)








### Electivity Species
## Electivity
electivity <- as.data.frame(matrix(NA,length(unique(df$Species)),length(unique(df$Primary.habitat))))
rownames(electivity) <- unique(df$Species)
colnames(electivity) <- unique(df$Primary.habitat)
for (qs in unique(df$Species)) {
  for (hab in unique(df$Primary.habitat)) {
    df.qs <- filter(df, Species==qs)
    df.qs.hab <- filter(df, Species==qs, Primary.habitat==hab)
    df.hab <- filter(Habitat, Habitat==hab)
    r <- sum(df.qs.hab$Total.animals)/sum(df.qs$Total.animals)
    p <- nrow(df.hab)/nrow(Habitat)
    electivity[[qs,hab]] <- (r-p)/(r+p)
  }
}
# View(electivity)

not_abs_bold <- function(V,a) {
  V[abs(V)>=a] <- ""
  return(V)
}
abs_bold <- function(V,a) {
  V[abs(V)<a] <- ""
  return(V)
}
electivity$Species <- rownames(electivity)
melectivity <- melt(electivity)
colnames(melectivity) <- c("Species","Habitat","E")
melectivity$Habitat <- factor(melectivity$Habitat, levels=c("OB","LB","MB"))

ggplot(melectivity, aes(x=Species, y=E, fill=Habitat))+
  theme_classic()+
  geom_col(position="dodge")+
  scale_fill_manual(labels=c("Open bush","Light bush","Medium bush"), values=c(ob,lb,mb))+
  labs(x="Species",y="Ivlev's Electivity Index (E')",fill="Habitat")+
  scale_x_discrete(limits=c("GZ","PZ","Cattle","MC","CC","CKC"),labels=c("Grevy's zebra","Plains zebra","Unknown Cattle","Mpala Cattle","Community Cattle","Community Kaparo Cattle"))+
  theme(legend.position="top")+
  geom_text(aes(label = abs_bold(round(E,3),0.1), y = E + 0.0365*(sign(E)-1), fontface=2), position = position_dodge(1), vjust = -0.5,family="Arial")+
  geom_text(aes(label = not_abs_bold(round(E,3),0.1), y = E + 0.0365*(sign(E)-1)), position = position_dodge(1), vjust = -0.5,family="Arial")+
  geom_rect(aes(xmin=0, xmax=Inf, ymin=-0.1, ymax=0.1), colour=NA, fill="grey", linetype="dashed", alpha = 0.04)


melectivity$Species <- factor(melectivity$Species, levels=c("GZ","PZ","Cattle","MC","CC","CKC"))
melectivity[melectivity$Species%in%camel.abbr] <- NULL
melectivity <- filter(melectivity, !is.na(Species))
ggplot(melectivity, aes(x=Habitat, y=E, fill=Species))+
  theme_classic()+
  geom_col(position="dodge")+
  scale_fill_manual(labels=c("Grevy's zebra","Plains zebra","Unknown Cattle","Mpala Cattle","Community Cattle","Community Kaparo Cattle"), values=c(gz,pz,"#333333","#666666","#999999","#bbbbbb"))+
  labs(x="Species",y="Ivlev's Electivity Index (E')",fill="Habitat")+
  scale_x_discrete(limits=c("OB","LB","MB"),labels=c("Open bush","Light bush","Medium bush"))+
  theme(legend.position="top")+
  geom_text(aes(label = abs_bold(round(E,3),0.1), y = E + 0.0365*(sign(E)-1), fontface=2), position = position_dodge(1), vjust = -0.5,family="Arial", size=3.3)+
  geom_text(aes(label = not_abs_bold(round(E,3),0.1), y = E + 0.0365*(sign(E)-1)), position = position_dodge(1), vjust = -0.5,family="Arial",size=3.3)+
  # geom_ribbon(aes(xmin=-Inf, xmax=Inf), ymin=-0.1, ymax=0.1))#, colour="grey", fill="grey", linetype="dashed", alpha = 1)
  geom_rect(aes(xmin=0, xmax=Inf, ymin=-0.1, ymax=0.1), colour=NA, fill="grey", linetype="dashed", alpha = 0.04)
# geom_hline(aes(yintercept=0.1,color="black", linetype="dashed"))+geom_hline(aes(yintercept=-0.1,color="black", linetype="dashed"))





############


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

















##############

cattle.abbr <- c("Cattle","CKC","CC","MC")
camel.abbr <- c("Camel","ZC","Comm_Camel")
zebra.abbr <- c("GZ","PZ")

# Z <- filter(df, QuickSpecies%in%c("GZ","PZ"))
Z <- filter(df, QuickSpecies=="GZ")
(hab.zeb <- kruskal.test(Total.animals ~ Primary.habitat, data=Z))
TukeyHSD(hab.zeb)

aov(Length ~ Group, data = df)


# Colours
# red <- "#fc1303"
# orange <- "#fc8403"
# yellow <- "#f7d114"
# green <- "#00c750"
# blue <- "#033dfc" #1644db

ob <- "#ffcc33"
lb <- "#a3c586"
mb <- "#5b7444"
gz <- "#a02923"
pz <- "#fdfdf6"
cattle <- "#aaaaaa"
camel <- "#333333"

# df.s <- filter(df,Species%in%c("GZ","PZ"))
df$Percent <- NA
df$Percent[df$QuickSpecies=="PZ"] <- df$Total.animals[df$QuickSpecies=="PZ"]/sum(df$Total.animals[df$QuickSpecies=="PZ"],na.rm=TRUE)
df$Percent[df$QuickSpecies=="GZ"] <- df$Total.animals[df$QuickSpecies=="GZ"]/sum(df$Total.animals[df$QuickSpecies=="GZ"],na.rm=TRUE)
df$Percent[df$QuickSpecies=="Cattle"] <- df$Total.animals[df$QuickSpecies=="Cattle"]/sum(df$Total.animals[df$QuickSpecies=="Cattle"],na.rm=TRUE)
# df$Total.animals[df$Species%in%cattle.abbr] <- df$Total.animals[df$Species%in%cattle.abbr]/10

# df$Percent <- NA
# for (row in 1:nrow(df)) {
#   df$Percent[row] <- df$Total.animals[row]/sum(df$Total.animals[df$QuickSpecies==df$QuickSpecies[row]],na.rm=TRUE)
# }


# df %>% arrange(factor(Primary.habitat, levels=c("OB","LB","MB")))


ggplot(df, aes(x=QuickSpecies, y=Percent, fill=Primary.habitat))+
  geom_violin(trim=FALSE)+
  # geom_boxplot(width=0.1)+
  # stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black")+
  theme_bw()+
  scale_color_manual(name="Species",values=c(lb,mb,ob))+
  scale_fill_manual(name="Species",values=c(lb,mb,ob))+
  scale_x_discrete(limits=c("PZ","GZ","Cattle"),labels=c("Plains zebra","Grevy's zebra","Cattle"))+#limits=c("OB","LB","MB"),labels=c("Open bush","Light bush","Medium bush"))+ # (scale: 10%)
  scale_y_continuous(labels = percent)+
  labs(x="Species",y="Size of dazzle/mob (percent total population)",fill="Habitat")+
  theme(legend.position="top")

ggplot(df, aes(x=Primary.habitat, y=Percent, fill=QuickSpecies))+
  geom_violin(trim=FALSE)+
  # geom_boxplot(width=0.1)+
  # stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black")+
  theme_bw()+
  scale_color_manual(name="Species", values=c(cattle,gz,pz))+
  scale_color_discrete(breaks=c("PZ","GZ","Cattle"),labels=c("Plains zebra","Grevy's zebra","Cattle"))+
  scale_fill_manual(values=c(cattle,gz,pz))+
  scale_x_discrete(limits=c("OB","LB","MB"),labels=c("Open bush","Light bush","Medium bush"))+#limits=c("OB","LB","MB"),labels=c("Open bush","Light bush","Medium bush"))+
  scale_y_continuous(labels = percent)+
  labs(x="Species",y="Size of dazzle/mob (percent total population)",fill="Habitat")+
  theme(legend.position="top")


# ggplot(df, aes(x=QuickSpecies, y=Total.animals, fill=Primary.habitat))+
# geom_boxplot(width=0.1)