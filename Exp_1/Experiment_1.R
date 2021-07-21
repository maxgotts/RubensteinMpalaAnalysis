df <- filter(df, is.before(mdy(Date),ymd("2021-07-05")))

df.before <- filter(df, is.before(mdy(df$Date),ymd("2021-06-14")))
df.while <- filter(df, is.strictly.after(mdy(df$Date),ymd("2021-06-14")), is.strictly.before(mdy(df$Date),ymd("2021-06-19")))
df.after <- filter(df, is.after(mdy(df$Date),ymd("2021-06-19")))
df.before$Exp.1 <- "before"
df.while$Exp.1 <- "while"
df.after$Exp.1 <- "after"
df <- rbind(df.before,df.while,df.after)
df$Exp.1 <- factor(df$Exp.1, levels=c("before","while","after"))
levels(df$Exp.1) <- c("Before","While","After")



df.before <- filter(df, is.before(mdy(df$Date),ymd("2021-06-16")))
df.after <- filter(df, is.after(mdy(df$Date),ymd("2021-06-17")))
df.before$Exp.1_2 <- "before"
df.after$Exp.1_2 <- "after"
df <- rbind(df.before,df.after)
df$Exp.1_2 <- factor(df$Exp.1_2, levels=c("before","after"))
levels(df$Exp.1_2) <- c("Before","After")




if (FALSE) {
  source('~/Desktop/MPALA/Analysis/Exp_1/Experiment_1.R')
}