rm(list=ls())

library(DescTools)
library(reshape2)

get_results <- function(p.t) {
  cat("Fisher's p =",fisher.test(p.t,simulate.p.value=TRUE)$p.value,"\n")
  cat("Chi-squared p =",chisq.test(p.t)$p.value, "\n")
  cat("G-test p =",GTest(p.t)$p.value, "\n")
}

source('~/Desktop/MPALA/mpala.R')
source('~/Desktop/MPALA/Analysis/Exp_1/Experiment_1.R')

df.pt <- df
df.pt <- filter(df, QuickSpecies=="PZ")
p.t <- matrix(NA,3,3)
colnames(p.t) <- unique(df.pt$Primary.habitat)
rownames(p.t) <- unique(df.pt$Exp.1)
for (habitat in colnames(p.t)) {
  for (phase in rownames(p.t)) {
    p.t[phase,habitat] <- sum(filter(df.pt, Primary.habitat==habitat, Exp.1==phase)$Total.animals,na.rm=T)
  }
}
# View(p.t)
# cat("Fisher's p =",fisher.test(p.t,simulate.p.value=TRUE)$p.value,"\n")
# cat("Chi-squared p =",chisq.test(p.t)$p.value, "\n")
# cat("G-test p =",GTest(p.t)$p.value, "\n")

get_results(p.t)


# How to standardize by man-hours looking for them? Loop-hours?

p.t.df <- as.data.frame(p.t)
p.t.df$Exp.1 <- rownames(p.t.df)
pt <- melt(p.t.df)
colnames(pt) <- c("Exp.1","Habitat","Total")

ggplot(pt, aes(x=Exp.1, y=Total, fill=Habitat))+
  theme_classic()+
  geom_col(position="dodge")+
  scale_fill_manual(labels=bush.names, values=c(og,lb,mb))+
  labs(x="Phase of experiment",y="Total individuals",fill="Habitat")+
  scale_x_discrete(limits=phase3.abbr,labels=phase3.names)+
  theme(legend.position="top")+
  geom_text(aes(label = Total, y = Total+0.05), position = position_dodge(1), vjust = -0.5,family="Arial")+
  # geom_ribbon(aes(xmin=-Inf, xmax=Inf), ymin=-0.1, ymax=0.1))#, colour="grey", fill="grey", linetype="dashed", alpha = 1)
  geom_rect(aes(xmin=0, xmax=Inf, ymin=-0.1, ymax=0.1), colour=NA, fill="grey", linetype="dashed", alpha = 0.04)
# geom_hline(aes(yintercept=0.1,color="black", linetype="dashed"))+geom_hline(aes(yintercept=-0.1,color="black", linetype="dashed"))




#####################################################################################
##---------------------------------------------------------------------------------##
##-----------------------------------Archive---------------------------------------##
##---------------------------------------------------------------------------------##
#####################################################################################
# q()

c.t <- matrix(NA,3,3)
colnames(c.t) <- bushland
rownames(c.t) <- levels(df$Exp.1)
for (habitat in colnames(c.t)) {
  for (phase in rownames(c.t)) {
    c.t[phase,habitat] <- sum(p.t[phase,])*sum(p.t[,habitat])/sum(p.t)
  }
}

pchisq(15, df=2, lower.tail=FALSE)
chi2 <- sum((p.t-c.t)^2/c.t)


pt.ft <- function(DF, Group1, Group2, Variable) {
  p.t <- matrix(NA,3,3)
  colnames(p.t) <- unique(DF[,Group1])
  rownames(p.t) <- unique(DF[,Group2])
  for (g1 in colnames(p.t)) {
    for (g2 in rownames(p.t)) {
      p.t[g2,g1] <- sum(DF[(DF[,Group1]==g1 & df[,Group2]==g2),Variable],na.rm=T)
    }
  }
  cat("Fisher's p =",fisher.test(p.t,simulate.p.value=TRUE)$p.value,"\n")
  cat("Chi-squared p =",chisq.test(p.t)$p.value, "\n")
}


pt.ft(df, "Exp.1","Primary.habitat", "Total.animals")
pt.ft(df %>% filter(QuickSpecies=="GZ"), "Exp.1","Primary.habitat", "Total.animals")
pt.ft(df %>% filter(QuickSpecies=="PZ"), "Exp.1","Primary.habitat", "Total.animals")
pt.ft(df %>% filter(QuickSpecies=="Cattle"), "Exp.1","Primary.habitat", "Total.animals")



df[,"VARIABLE"] <- sample(c(0,1), nrow(df), replace=T)
df[,"GROUP1"] <- "a"
df[sample(nrow(df),100,replace=T),"GROUP1"] <- "b"
df[sample(nrow(df),100,replace=T),"GROUP1"] <- "c"
df[sample(nrow(df),100,replace=T),"GROUP1"] <- "d"
df[,"GROUP2"] <- "a"
df[sample(nrow(df),100,replace=T),"GROUP2"] <- "b"
df[sample(nrow(df),100,replace=T),"GROUP2"] <- "c"
df[sample(nrow(df),100,replace=T),"GROUP2"] <- "d"
pt.ft(df, "GROUP1","GROUP2","VARIABLE")