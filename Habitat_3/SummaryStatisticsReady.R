rm(list=ls())

library(reshape2)
library(scales)
library(lubridate)
library(gridExtra)
library(ggpmisc)

source('~/Desktop/MPALA/mpala.R')
df <- get_df()
ss <- get_ss()


x_days <- seq(1,max(ss$Days),5)   #3)
pre.x_days_value <- as.Date(0,origin="1970-01-01")+as.numeric(ymd("2021-06-03"))+x_days-1
x_days_value <- paste0(day(pre.x_days_value)," ",months(pre.x_days_value),", 2021")

begin_phase3 <- list(before=as.numeric(mdy("6/3/21"))-as.numeric(mdy("6/3/21"))+1-0.5,
                     during=as.numeric(mdy("6/14/21"))-as.numeric(mdy("6/3/21"))+1,
                     after=as.numeric(mdy("6/18/21"))-as.numeric(mdy("6/3/21"))+1,
                     end=as.numeric(mdy("7/5/21"))-as.numeric(mdy("6/3/21"))+1)
end_phase3 <- list(before=as.numeric(mdy("6/14/21"))-as.numeric(mdy("6/3/21"))+1,
                   during=as.numeric(mdy("6/18/21"))-as.numeric(mdy("6/3/21"))+1,
                   after=as.numeric(mdy("7/5/21"))-as.numeric(mdy("6/3/21"))+1,
                   end=max(ss$Days)+0.5)

if (!is.null(ss$Size)) ss$Size <- factor(ss$Size,levels=c("Small","Medium","Big"))

if (FALSE) {
  source('~/Desktop/MPALA/Analysis/Habitat_3/SummaryStatisticsReady.R')
}