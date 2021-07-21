rm(list=ls())


source('~/Desktop/MPALA/mpala.R')
source('~/Desktop/MPALA/Analysis/Exp_1/Experiment_1.R')
df <- filter(df, !is.na(Total.animals))
hb <- read.csv("~/Desktop/MPALA/Maps/TIFFs.csv")


hb.sample.before <- hb %>% sample_n(5000) %>% filter(!is.na())
for (pixel_id in hb.sample) {
  
}