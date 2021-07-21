rm(list=ls())

source('~/Desktop/MPALA/mpala.R')

df$Tree.cover <- as.numeric(find_replace(df$Primary.habitat, data.frame(x=bushland,y=c(.1,.5,.9))))
df$Tree.cover.2 <- as.numeric(find_replace(df$Secondary.habitat, data.frame(x=bushland,y=c(.1,.5,.9))))
df$Tree.cover.3 <- as.numeric(as.numeric(find_replace(df$Tertiary.habitat, data.frame(x=bushland,y=c(.1,.5,.9)))))
df$Sun <- as.numeric(find_replace(df$Sun, data.frame(x=c("no sun","part","full",""),y=c(0,.5,1,NA))))
df$Wind <- as.numeric(find_replace(df$Wind, data.frame(x=c("no wind","light","medium","strong",""),y=c(0,.3,.6,.9,NA))))
df$Grass.height <- as.numeric(find_replace(df$Grass.height, data.frame(x=c("hoof","hock","belly","Lh",""),y=c(.25,.5,.75,NA,NA))))
df$Grass.color <- as.numeric(find_replace(df$Grass.color, data.frame(x=c("brown","brown with some green","green with some brown","green",""),y=c(.1,.4,.7,1,NA))))
rownames(df) <- df$Identifier
# df$Group.composition <- factor(df$Multispecies, levels=c(0:1,NA))

# Time, Longitude, Latitude, Sun, Wind, Grass.height, Grass.color

gz <- filter(df, Species=="GZ")
pz <- filter(df, Species=="PZ")
zebras <- filter(df, Species%in%zebra.abbr)
cattle <- filter(df, QuickSpecies=="Cattle")

mylogit <- glm(Multispecies ~ Distance.from.mob + Tree.cover, data = df, family = "binomial")
