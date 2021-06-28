rm(list=ls())

VIEW <- FALSE

install.packages("~/fitnesslandscapes2", 
                 repos = NULL, 
                 type = "source")
library(fitnesslandscapes2)
library(dplyr)
library(ggbiplot)

df <- read.csv("~/Desktop/MPALA/Whitesheets/Gotts_ConvertedWhitesheets.csv")
df$Identifier <- paste0(df$Species,"_",df$Whitesheet.Filename,"_",df$Whitesheet.Entry.Number)
df$Photos.begin <- NULL
df$Photos.end <- NULL
df$Multispecies <- NULL
df$Loop <- NULL
df$Whitesheet.Entry.Number <- NULL
df$Whitesheet.Filename <- NULL
df <- df[,c(1:17,30:40,44)]
df <- filter(df, !is.na(Total.animals), Species!="Camel", !is.na(Distance.from.water))

df$Species.backup <- df$Species
for (colm in colnames(df)) {
  if (is.na(match(colm,c("Identifier", "Species.backup", "Latitude", "Longitude", "Year", "Month", "Day", "Date", "Time", "Hour", "Minute", "Total.animals", "Number.grasses")))) { 
    df[,colm] <- as.integer(as.factor(df[,colm]))
  }
}

if (VIEW) View(df[,c("Species","Species.backup")])
df$Species.backup <- NULL

df$Other.species.3 <- NULL # Since it's empty right now

# df <- df[df$Longitude > quantile(df$Longitude, 0.25), ]
# df <- df[df$Latitude > quantile(df$Latitude, 0.25), ]

PZ <- 3
GZ <- 2
Cattle <- 1

df_gz <- subset(df, Species == GZ)
df_pz <- subset(df, Species == PZ)
df_cattle <- subset(df, Species == Cattle)
df_zebras <- subset(df, Species != Cattle)

df_gz$Species <- NULL
df_pz$Species <- NULL
df_cattle$Species <- NULL



















#### PPR #### 

EXCLUDES <- c("Identifier","Latitude","Longitude","Year","Month","Day", "Date","Time","Minute","Rain") #Activity
EXCLUDES_saved <- EXCLUDES

if ("Total.animals" %in% EXCLUDES) EXCLUDES <- EXCLUDES_saved #cat("Warning: \"Total.animals\" in EXCLUDES")

PPR_zebras <- DimReduction(df_zebras,TYPE="PPR",EXCLUDE=EXCLUDES,VARIABLE="Total.animals")
PPR_zebras$ppr$gof
PPR_zebras$ppr$beta
if (VIEW) View(abs(PPR_zebras$weights))
df_zebras[,c("PP1","PP2")] <- PPR_zebras$columns

PPR_gz <- DimReduction(df_gz,TYPE="PPR",EXCLUDE=EXCLUDES,VARIABLE="Total.animals")
PPR_zebras$ppr$gof
PPR_zebras$ppr$beta
if (VIEW) View(abs(PPR_gz$weights))
df_gz[,c("PP1","PP2")] <- PPR_gz$columns

PPR_pz <- DimReduction(df_pz,TYPE="PPR",EXCLUDE=EXCLUDES,VARIABLE="Total.animals")
PPR_pz$ppr$gof
PPR_pz$ppr$beta
if (VIEW) View(abs(PPR_pz$weights))
df_pz[,c("PP1","PP2")] <- PPR_pz$columns

PPR_cattle <- DimReduction(df_cattle,TYPE="PPR",EXCLUDE=EXCLUDES,VARIABLE="Total.animals")
PPR_cattle$ppr$gof
PPR_cattle$ppr$beta
if (VIEW) View(abs(PPR_cattle$weights))
df_cattle[,c("PP1","PP2")] <- PPR_cattle$columns



### PCA ###
EXCLUDES <- c(EXCLUDES, "Total.animals")

PCA_zebras <- DimReduction(df_zebras,TYPE="PCA",EXCLUDE=EXCLUDES)
if (VIEW) View(PCA_zebras$columns)
df_zebras[,c("PC1","PC2")] <- PCA_zebras$columns

PCA_gz <- DimReduction(df_gz,TYPE="PCA",EXCLUDE=EXCLUDES)
if (VIEW) View(PCA_gz$columns)
df_gz[,c("PC1","PC2")] <- PCA_gz$columns

PCA_pz <- DimReduction(df_pz,TYPE="PCA",EXCLUDE=EXCLUDES)
if (VIEW) View(PCA_pz$columns)
df_pz[,c("PC1","PC2")] <- PCA_pz$columns

PCA_cattle <- DimReduction(df_cattle,TYPE="PCA",EXCLUDE=EXCLUDES)
if (VIEW) View(PCA_cattle$columns)
df_cattle[,c("PC1","PC2")] <- PCA_cattle$columns


### LDA ###
if (!("Total.animals" %in% EXCLUDES)) EXCLUDES <- c(EXCLUDES, "Total.animals")

LDA_zebras <- DimReduction(df_zebras,TYPE="LDA",EXCLUDE=EXCLUDES, VARIABLE="Species")
if (VIEW) View(LDA_zebras$columns)
df_zebras[,"LD1"] <- LDA_zebras$columns

LDA_df <- DimReduction(df,TYPE="LDA",EXCLUDE=EXCLUDES, VARIABLE="Species")
if (VIEW) View(LDA_df$columns)
df[,c("LD1","LD2")] <- LDA_df$columns




### LANDSCAPES ###

TPS_landscape(df,output="contour",z="Total.animals",Lambda="special",x="LD1",y="LD2")


TPS_landscape(df_zebras,output="contour",z="Total.animals",Lambda="default")
TPS_distribution(df_zebras,output="contour",Lambda="default")

TPS_landscape(df_zebras,output="contour",z="Total.animals", x="PP1", y="PP2", Lambda="default")









TPS_landscape(df_zebras,output="contour",z="Total.animals",Lambda="special")

TPS_landscape(df_gz,output="contour",z="Total.animals",Lambda="default")
TPS_landscape(df_gz,output="contour",x="PC1",y="PC2",z="Total.animals",Lambda="default")

TPS_landscape(df_pz,output="contour",z="Total.animals",Lambda="default")
TPS_landscape(df_cattle,output="contour",z="Total.animals")
TPS_landscape(subset(df_zebras,Species==GZ),output="contour",z="Total.animals",Lambda="default")
TPS_landscape(subset(df_zebras,Species==PZ),output="contour",z="Total.animals",Lambda="default")


TPS_landscape(df_gz,output="contour",z="Total.animals",x="Latitude",y="Hour")



TPS_landscape(df_pz,output="contour",FitnessMetric="Total.animals",VarName1="PC1",VarName2="PC2")#,Lambda="default")
TPS_landscape(df_cattle,output="contour",FitnessMetric="Total.animals",VarName1="PC1",VarName2="PC2")#,Lambda="default")
TPS_landscape(df_zebras,output="contour",FitnessMetric="Total.animals",VarName1="PC1",VarName2="PC2")#,Lambda="default")
TPS_landscape(filter(df_zebras,Species==GZ),output="contour",FitnessMetric="Total.animals",VarName1="PC1",VarName2="PC2")#,Lambda="default")
TPS_landscape(filter(df_zebras,Species==PZ),output="contour",FitnessMetric="Total.animals",VarName1="PC1",VarName2="PC2")#,Lambda="default")
#


### VISUALIZATION
royal <- "#34568B"
uv <- "#6B5B95"
emerald <- "#88B04B"

TPS_landscape(df_zebras,output="contour",z="Total.animals", x="PP1", y="LD1", Lambda="special")
ggplot(data=df_zebras, mapping=aes(x=PP1, y=LD1, color=as.factor(Species)))+
  stat_ellipse(aes(x=PP1, y=LD1, fill = as.factor(Species)), alpha = 0.2, geom = "polygon")+
  geom_point()+
  theme_bw()+
  labs(x="PP1",y="LD1")+
  scale_color_manual(name="Species",values=c(uv,emerald),breaks=c(2,3),labels=c("GZ","PZ"))+
  scale_fill_manual(name="Species",values=c(uv,emerald),breaks=c(2,3),labels=c("GZ","PZ"))













### PCA vs PPR ANALYSIS ###

df_zebras$Species <- as.factor(df_zebras$Species)
ggbiplot(PCA_zebras,ellipse=TRUE, groups=df_zebras$Species)
ggbiplot(PCA_gz)
ggbiplot(PCA_pz)
ggbiplot(PCA_cattle)


##




























































#### BOLTZMANN LANDSCAPE ####

Utility <- function(Population) {
  return((0.98*log(Population)+log(max(Population)^0.01/min(Population)^0.99))/log(max(Population)/min(Population)))
}


df_gz$Utility <- Utility(df_gz$Total.animals)
df_pz$Utility <- Utility(df_pz$Total.animals)
df_cattle$Utility <- Utility(df_cattle$Total.animals)



EXCLUDES <- c("Identifier", "Latitude","Longitude","Year","Month","Day", "Date","Time","Minute","Activity","Total.animals") #,"PP1","PP2")

# PPR_all <- PPR_LDA(df,FITNESS="Utility",exclude=EXCLUDES)
# PPR_all[[2]]
# PPR_all[[2]]$beta
# if (VIEW) View(abs(PPR_all[[3]]))
# df[,c("PP1","PP2")] <- PPR_all[[1]][,c("PP1","PP2")]

PPR_gz <- PPR_LDA(df_gz,FITNESS="Utility",exclude=EXCLUDES)
PPR_gz[[2]]
PPR_gz[[2]]$beta
if (VIEW) View(abs(PPR_gz[[3]]))
df_gz[,c("PP1","PP2")] <- PPR_gz[[1]][,c("PP1","PP2")]

PPR_pz <- PPR_LDA(df_pz,FITNESS="Utility",exclude=EXCLUDES)
PPR_pz[[2]]
PPR_pz[[2]]$beta
if (VIEW) View(abs(PPR_pz[[3]]))
df_pz[,c("PP1","PP2")] <- PPR_pz[[1]][,c("PP1","PP2")]

PPR_cattle <- PPR_LDA(df_cattle,FITNESS="Utility",exclude=c(EXCLUDES,"Rain"))
PPR_cattle[[2]] ## DOESN'T WORK AS WELL -- as expected!
PPR_cattle[[2]]$beta
if (VIEW) View(abs(PPR_cattle[[3]]))
df_cattle[,c("PP1","PP2")] <- PPR_cattle[[1]][,c("PP1","PP2")]

# TPS_landscape(df,output="contour",FitnessMetric="Utility")
TPS_landscape(df_gz,output="contour",FitnessMetric="Utility")
TPS_landscape(df_pz,output="contour",FitnessMetric="Utility")
TPS_landscape(df_cattle,output="contour",FitnessMetric="Utility")











#### VIOLIN & HIST PLOTS ####


library(ggplot2)

ggplot(df_gz, aes(x=Total.animals)) + 
  geom_histogram(binwidth=1,color="black", fill="white")+labs(x="Population",y="Count")+theme_bw()
ggplot(df_pz, aes(x=Total.animals)) + 
  geom_histogram(binwidth=1,color="black", fill="white")+labs(x="Population",y="Count")+theme_bw()
ggplot(df_cattle, aes(x=Total.animals)) + 
  geom_histogram(binwidth=1,color="black", fill="white")+labs(x="Population",y="Count")+theme_bw()

ggplot(df_gz, aes(Total.animals))+
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1)+
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="black")+
  theme_bw()
