rm(list=ls())

source('~/Desktop/MPALA/mpala.R')

library(reshape2)
library(scales)
library(ggpmisc)

ss <- get_ss()
df <- get_df()
vi <- get_vi()

source('~/Desktop/MPALA/Analysis/Exp_1/Experiment_1.R')

ss.zebra <- ss %>% filter(Species %in% zebra.abbr, Exp.1!="Not in experiment")
df.zebra <- df %>% filter(Species %in% zebra.abbr, Exp.1!="Not in experiment")

if (FALSE) {
  source('~/Desktop/MPALA/Analysis/Final_Project/finalSetup.R')
}