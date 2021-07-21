rm(list=ls())

source('~/Desktop/MPALA/mpala.R')

library(reshape2)
library(scales)
library(ggpmisc)

ss <- get_ss()
df <- get_df()
vi <- get_vi()

ss.zebra <- ss %>% filter(Species %in% zebra.abbr)
df.zebra <- df %>% filter(Species %in% zebra.abbr)

if (FALSE) {
  source('~/Desktop/MPALA/Analysis/Final_Project/playgroundSetup.R')
}