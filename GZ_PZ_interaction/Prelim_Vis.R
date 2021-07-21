rm(list=ls())

mean_plus_sd <- function(vec) mean(vec)+sd(vec)
mean_minus_sd <- function(vec) mean(vec)-sd(vec)

library(fitnesslandscapes2)
library(ggplot2)
library(sm)

source('~/Desktop/MPALA/mpala.R')
source('~/Desktop/MPALA/Analysis/Exp_1/Experiment_1.R')
df <- filter(df.exp.1, !(Identifier%in%blacklist))

zebra <- filter(df, Species%in%zebra.abbr) #, Loop%in%1:2)

zebra$Multispecies <- factor(find_replace(zebra$Multispecies, data.frame(x=c(0,1),y=c("ind","multi"))), levels=c("ind","multi"))
zebra$Species <- factor(zebra$Species, levels=c("GZ","PZ"))
zebra$Exp.1 <- factor(zebra$Exp.1, levels=phase3.abbr)
levels(zebra$Exp.1) <- phase3.names

#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#
#-----------------------------------------Bar Plots---------------------------------------------#
#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#
Population <- data.frame(Species=rep(zebra$Species),times=3, Exp.1=rep(zebra$Exp.1, each=2),  Totals=NA)
for (row in 1:nrow(Population)) Population[row,"Totals"] <- sum(filter(zebra, Species==Population[row,"Species"], Exp.1==Population[row,"Exp.1"])$Total.animals)
ggplot(Population, aes(x=Species, y=Totals, fill=Exp.1))+
  theme_classic()+
  geom_col(position="dodge")+
  scale_fill_manual(labels=phase3.names, values=phase3.cols)+
  labs(x="Species",y="Total animals",fill="Phase of experiment")+
  scale_x_discrete(limits=zebra.abbr,labels=zebra.names)+
  geom_text(aes(label = Totals, y = Totals + 0.05), position = position_dodge(1), vjust = -0.5, family="Arial",size=3.3)+
  theme(legend.position="top")



#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#
#-------------------------------Incredible Violin Plots-----------------------------------------#
#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#
dodge <- position_dodge(width = 1)

#-----------------------------------------------------------------------------------------------#
#-----------------------------------Incredible by Herd Size-------------------------------------#
#-----------------------------------------------------------------------------------------------#
ggplot(data = zebra, aes(x=Species, y=as.numeric(Total.animals), fill=Multispecies))+
  annotate(geom="rect",xmin=0.5, xmax=1.5, ymin=0, ymax=Inf, fill=gz.2, alpha=0.25)+
  annotate(geom="rect", xmin=1.5, xmax=2.5, ymin=0, ymax=Inf, fill=pz.2, alpha=0.25)+
  geom_violin(position = dodge, trim=T)+
  geom_boxplot(width=0.1, outlier.colour=NA, position = dodge)+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black", position=dodge, aes(g=Multispecies))+
  stat_summary(fun=mean_plus_sd, geom="point", shape=1, size=3, fill="black", position=dodge, aes(g=Multispecies))+
  stat_summary(fun=mean_minus_sd, geom="point", shape=1, size=3, fill="black", position=dodge, aes(g=Multispecies))+
  theme_classic()+
  scale_x_discrete(limits=zebra.abbr,labels=zebra.names)+
  scale_fill_manual(labels=multispecies.names, values=multispecies.cols)+
  labs(x="Species",y="Herd size (individuals)",fill="Group composition")+
  theme(legend.position="top")+
  facet_wrap(~Exp.1)

#-----------------------------------------------------------------------------------------------#
#------------------------------Incredible by Distance from mob----------------------------------#
#-----------------------------------------weighted----------------------------------------------#
#-----------------------------------------------------------------------------------------------#
ggplot(data = filter(duplicate.rows(zebra), !is.na(Exp.1)), aes(x=Species, y=as.numeric(Distance.from.mob), fill=Multispecies))+
  annotate(geom="rect",xmin=0.5, xmax=1.5, ymin=0, ymax=Inf, fill=gz.2, alpha=0.25)+
  annotate(geom="rect", xmin=1.5, xmax=2.5, ymin=0, ymax=Inf, fill=pz.2, alpha=0.25)+
  geom_violin(position = dodge, trim=T)+
  geom_boxplot(width=0.1, outlier.colour=NA, position = dodge)+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black", position=dodge, aes(g=Multispecies))+
  stat_summary(fun=mean_plus_sd, geom="point", shape=1, size=3, fill="black", position=dodge, aes(g=Multispecies))+
  stat_summary(fun=mean_minus_sd, geom="point", shape=1, size=3, fill="black", position=dodge, aes(g=Multispecies))+
  theme_classic()+
  scale_x_discrete(limits=zebra.abbr,labels=zebra.names)+
  scale_fill_manual(labels=multispecies.names, values=multispecies.cols)+
  labs(x="Species",y="Distance from mob (m)",fill="Group composition")+
  theme(legend.position="top")+
  facet_wrap(~Exp.1)


#-----------------------------------------------------------------------------------------------#
#-------------------------------Incredible by Tree Cover----------------------------------------#
#-----------------------------------------weighted----------------------------------------------#
#-----------------------------------------------------------------------------------------------#
# zebra$Primary.habitat <- factor(zebra$Primary.habitat, levels=bushland)
# zebra$TreeCover <- as.integer(zebra$Primary.habitat)
zebra$TreeCover <- find_replace(zebra$Primary.habitat, data.frame(x=bushland,y=c(0.1,0.5,0.9)))
zebra.dup <- filter(duplicate.rows(zebra), !is.na(Exp.1))
ggplot(data = zebra.dup, aes(x=Species, y=as.numeric(TreeCover), fill=Multispecies))+
  annotate(geom="rect",xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, fill=gz.2, alpha=0.25)+
  annotate(geom="rect", xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, fill=pz.2, alpha=0.25)+
  geom_violin(position = dodge, trim=F)+
  geom_boxplot(width=0.1, outlier.colour=NA, position = dodge)+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black", position=dodge, aes(g=Multispecies))+
  stat_summary(fun=mean_plus_sd, geom="point", shape=1, size=3, fill="black", position=dodge, aes(g=Multispecies))+
  stat_summary(fun=mean_minus_sd, geom="point", shape=1, size=3, fill="black", position=dodge, aes(g=Multispecies))+
  theme_classic()+
  scale_x_discrete(limits=zebra.abbr,labels=zebra.names)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(labels=multispecies.names, values=multispecies.cols)+
  labs(x="Species",y="Tree Cover",fill="Group composition")+
  theme(legend.position="top")+
  facet_wrap(~Exp.1)




#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#
#------------------------------ANCOVA and ANOVA on above----------------------------------------#
#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------#
#---------------------------------------KW/MWU by Herd Size-------------------------------------#
#-----------------------------------------------------------------------------------------------#
Anova(aov(Total.animals ~ Species + Multispecies + Exp.1, zebra), type="II")
Anova(aov(Distance.from.mob ~ Species + Multispecies + Exp.1, filter(duplicate.rows(zebra), !is.na(Exp.1))), type="II")

zebra$Group <- paste(zebra$Species,zebra$Multispecies,zebra$Exp.1,sep="-")
kruskal.test(Total.animals ~ Group, zebra)
m <- matrix(1, length(unique(zebra$Group)), length(unique(zebra$Group)))
n <- matrix("", length(unique(zebra$Group)), length(unique(zebra$Group)))
for (i in 1:length(unique(zebra$Group))) {
  for (j in (i+1):length(unique(zebra$Group))) {
    if (j > length(unique(zebra$Group))) break
    two.groups <- unique(zebra$Group)[c(i,j)]
    p <- wilcox.test(Total.animals ~ Group, zebra %>% filter(Group %in% two.groups))$p.value
    cat(two.groups,"p=",p,find_replace((p<=0.05), data.frame(x=c(T,F),y=c("*",""))),"\n")
    m[i,j] <- m[j,i] <- p
    if (p <= 0.05) n[i,j] <- n[j,i] <- round(p,4)
  }
}
rownames(m) <- colnames(m) <- rownames(n) <- colnames(n) <- unique(zebra$Group)
View(m)
View(n)



#-----------------------------------------------------------------------------------------------#
#----------------------------------KW/MWU by Distance from mob----------------------------------#
#-----------------------------------------weighted----------------------------------------------#
#-----------------------------------------------------------------------------------------------#
zebra <- duplicate.rows(zebra)
zebra <- zebra %>% filter(!is.na(Species), !is.na(Multispecies), !is.na(Exp.1))
zebra$Distance.from.mob <- as.numeric(zebra$Distance.from.mob)
zebra$Group <- paste(zebra$Species,zebra$Multispecies,zebra$Exp.1,sep="-")
kruskal.test(Distance.from.mob ~ Group, zebra)
m <- matrix(1, length(unique(zebra$Group)), length(unique(zebra$Group)))
n <- matrix("", length(unique(zebra$Group)), length(unique(zebra$Group)))
for (i in 1:length(unique(zebra$Group))) {
  for (j in (i+1):length(unique(zebra$Group))) {
    if (j > length(unique(zebra$Group))) break
    two.groups <- unique(zebra$Group)[c(i,j)]
    p <- wilcox.test(Distance.from.mob ~ Group, zebra %>% filter(Group %in% two.groups))$p.value
    cat(two.groups,"p=",p,find_replace((p<=0.05), data.frame(x=c(T,F),y=c("*",""))),"\n")
    m[i,j] <- m[j,i] <- p
    if (p <= 0.05) n[i,j] <- n[j,i] <- round(p,4)
  }
}
rownames(m) <- colnames(m) <- rownames(n) <- colnames(n) <- unique(zebra$Group)
View(m)
View(n)




















#-----------------------------------------------------------------------------------------------#
#----------------------------------------Archive------------------------------------------------#
#-----------------------------------------------------------------------------------------------#

# pairwise.kw.u <- function(DF, Variable, Group, VERBOSE=FALSE) {
#   KW <- kruskal.test(as.formula(paste(Variable, "~", Group)), DF)
#   m <- matrix(1, length(unique(DF[,Group])), length(unique(DF[,Group])))
#   n <- matrix("", length(unique(DF[,Group])), length(unique(DF[,Group])))
#   for (i in 1:length(unique(DF[,Group]))) {
#     for (j in (i+1):length(unique(DF[,Group]))) {
#       if (j > length(unique(DF[,Group]))) break
#       two.groups <- unique(DF[,Group])[c(i,j)]
#       p <- wilcox.test(as.formula(paste(Variable, "~", Group), DF[DF[,Group]%in%two.groups,]))$p.value
#       if (VERBOSE) cat(two.groups,"p=",p,find_replace((p<=0.05), data.frame(x=c(T,F),y=c("*",""))),"\n")
#       m[i,j] <- m[j,i] <- p
#       if (p <= 0.05) n[i,j] <- n[j,i] <- round(p,4)
#     }
#   }
#   rownames(m) <- colnames(m) <- rownames(n) <- colnames(n) <- unique(zebra$Group)
#   View(m)
#   View(n)
#   return(list(KW=KW, U=m, U.summary=n))
# }
